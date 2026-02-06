# client_eff.py
import os
import argparse
import numpy as np
import tensorflow as tf
import flwr as fl

try:
    import tenseal as ts
except ImportError:
    ts = None

from tensorflow.keras.applications import EfficientNetB0
from tensorflow.keras import layers, models

HE_FRACTION  = 0.10
SCRAMBLE_KEY = np.uint32(0xDEADBEEF)

#  Gaussian standard deviation (sigma)
NOISE_SCALE  = 0.05

CHUNK_SIZE   = 8192   # must match server


def _key_to_seed_u64(key: np.uint32) -> int:
    return int(key) & 0xFFFFFFFFFFFFFFFF


def permute_uint32(u32: np.ndarray, key: np.uint32) -> np.ndarray:
    """Deterministic keyed permutation over uint32 words."""
    flat = u32.reshape(-1)
    rng = np.random.default_rng(_key_to_seed_u64(key))
    perm = rng.permutation(flat.size)
    return flat[perm].reshape(u32.shape)


def unpermute_uint32(u32_scr: np.ndarray, key: np.uint32) -> np.ndarray:
    """Invert deterministic keyed permutation (recompute perm from key and length)."""
    flat = u32_scr.reshape(-1)
    rng = np.random.default_rng(_key_to_seed_u64(key))
    perm = rng.permutation(flat.size)
    inv = np.empty_like(perm)
    inv[perm] = np.arange(perm.size)
    return flat[inv].reshape(u32_scr.shape)


def build_model():
    base = EfficientNetB0(
        input_shape=(32, 32, 3),
        include_top=False,
        weights=None,
        pooling="avg",
    )
    out = layers.Dense(10, activation="softmax")(base.output)
    model = models.Model(inputs=base.input, outputs=out)
    model.compile(
        optimizer="adam",
        loss="sparse_categorical_crossentropy",
        metrics=["accuracy"],
    )
    return model


class FASClient(fl.client.Client):
    def __init__(self, cid: int, total_clients: int):
        self.model = build_model()
        self.cid = cid

        (x_train, y_train), (x_test, y_test) = tf.keras.datasets.cifar10.load_data()
        x_train = x_train.astype("float32")
        x_test  = x_test.astype("float32")

        if total_clients > 1:
            part = len(x_train) // total_clients
            start = cid * part
            end   = start + part if cid < total_clients - 1 else len(x_train)
            self.x_train = x_train[start:end]
            self.y_train = y_train[start:end]
            self.x_test  = x_test[start:end] if len(x_test) >= end else x_test
            self.y_test  = y_test[start:end] if len(y_test) >= end else y_test
        else:
            self.x_train, self.y_train = x_train, y_train
            self.x_test, self.y_test   = x_test, y_test

        self.param_shapes = [w.shape for w in self.model.get_weights()]
        self.total_len = int(sum(np.prod(s) for s in self.param_shapes))
        self.enc_len   = int(HE_FRACTION * self.total_len)
        self.num_chunks = (self.enc_len + CHUNK_SIZE - 1) // CHUNK_SIZE

        # TenSEAL context setup
        if ts is not None:
            if os.path.exists("context.sec"):
                with open("context.sec", "rb") as f:
                    self.context = ts.context_from(f.read())
            else:
                self.context = ts.context(
                    ts.SCHEME_TYPE.CKKS,
                    poly_modulus_degree=16384,
                    coeff_mod_bit_sizes=[40, 20, 40],
                )
                self.context.global_scale = 2**20
                self.context.generate_galois_keys()
                self.context.generate_relin_keys()
                with open("context.sec", "wb") as f:
                    f.write(self.context.serialize(save_secret_key=True))
                with open("context.pub", "wb") as f:
                    f.write(self.context.serialize(save_secret_key=False))
        else:
            self.context = None

    def _decode_fas_parameters(self, parameters: fl.common.Parameters):
        tensors = parameters.tensors
        if len(tensors) != self.num_chunks + 1:
            raise ValueError(
                f"Expected {self.num_chunks+1} tensors (HE chunks + DP part), got {len(tensors)}"
            )

        he_chunks_bytes = tensors[:self.num_chunks]
        rest_bytes      = tensors[self.num_chunks]

        # Decrypt HE chunks (client has secret key; ok)
        he_chunks = []
        for c_bytes in he_chunks_bytes:
            if ts is not None:
                enc_vec = ts.ckks_vector_from(self.context, c_bytes)
                dec = np.array(enc_vec.decrypt(), dtype=np.float32)
            else:
                dec = np.frombuffer(c_bytes, dtype=np.float32)
            he_chunks.append(dec)

        he_dec = np.concatenate(he_chunks, axis=0)

        # Unscramble DP part: keyed permutation inverse over uint32 words
        rest_uint32_scr = np.frombuffer(rest_bytes, dtype=np.uint32)
        if rest_uint32_scr.size > 0:
            rest_uint32 = unpermute_uint32(rest_uint32_scr, SCRAMBLE_KEY)
            rest_dec = rest_uint32.view(np.float32)
        else:
            rest_dec = np.array([], dtype=np.float32)

        flat = np.concatenate([he_dec, rest_dec], axis=0)

        # Rebuild weights
        new_weights = []
        offset = 0
        for shape in self.param_shapes:
            size = int(np.prod(shape))
            new_weights.append(flat[offset:offset+size].reshape(shape))
            offset += size

        return new_weights

    def get_parameters(self, ins: fl.common.GetParametersIns):
        weights = self.model.get_weights()
        parameters = fl.common.ndarrays_to_parameters(weights)
        return fl.common.GetParametersRes(
            status=fl.common.Status(fl.common.Code.OK, "Initial parameters sent"),
            parameters=parameters,
        )

    def fit(self, ins: fl.common.FitIns):
        parameters = ins.parameters

        # Receive global model
        if parameters.tensor_type == "numpy.ndarray":
            new_weights = fl.common.parameters_to_ndarrays(parameters)
        else:
            new_weights = self._decode_fas_parameters(parameters)

        self.model.set_weights(new_weights)

        # Local training
        self.model.fit(self.x_train, self.y_train, epochs=20, batch_size=32, verbose=0)

        # Flatten updated weights
        updated_weights = self.model.get_weights()
        flat = np.concatenate([w.flatten() for w in updated_weights]).astype(np.float32)

        enc_len = self.enc_len
        he_vec  = flat[:enc_len]
        rest_vec = flat[enc_len:]

        # Encrypt HE fraction in chunks
        he_chunks_bytes = []
        for j in range(self.num_chunks):
            start = j * CHUNK_SIZE
            end   = min(enc_len, (j + 1) * CHUNK_SIZE)
            chunk = he_vec[start:end]
            if ts is not None:
                enc_vec = ts.ckks_vector(self.context, chunk.tolist())
                c_bytes = enc_vec.serialize()
            else:
                c_bytes = chunk.tobytes()
            he_chunks_bytes.append(c_bytes)

        # Apply Gaussian noise + keyed permutation scrambling to rest
        if rest_vec.size > 0:
            noise = np.random.normal(
                loc=0.0, scale=NOISE_SCALE, size=rest_vec.shape
            ).astype(np.float32)

            rest_noisy = (rest_vec + noise).astype(np.float32)

            # Scramble = keyed permutation over uint32 representation
            rest_uint32 = rest_noisy.view(np.uint32)
            scrambled_rest = permute_uint32(rest_uint32, SCRAMBLE_KEY)
            rest_bytes = scrambled_rest.tobytes()
        else:
            rest_bytes = b""

        tensors = he_chunks_bytes + [rest_bytes]

        return fl.common.FitRes(
            status=fl.common.Status(fl.common.Code.OK, "Fit successful"),
            parameters=fl.common.Parameters(
                tensors=tensors,
                tensor_type="encrypted",
            ),
            num_examples=len(self.x_train),
            metrics={},
        )

    def evaluate(self, ins: fl.common.EvaluateIns):
        parameters = ins.parameters

        if parameters.tensor_type == "numpy.ndarray":
            eval_weights = fl.common.parameters_to_ndarrays(parameters)
        else:
            eval_weights = self._decode_fas_parameters(parameters)

        self.model.set_weights(eval_weights)
        loss, accuracy = self.model.evaluate(self.x_test, self.y_test, verbose=0)
        return fl.common.EvaluateRes(
            status=fl.common.Status(fl.common.Code.OK, "Evaluate successful"),
            loss=float(loss),
            num_examples=len(self.x_test),
            metrics={"accuracy": float(accuracy)},
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--cid", type=int, default=0, help="Client ID")
    parser.add_argument("--total_clients", type=int, default=1, help="Total number of clients")
    parser.add_argument("--server_address", type=str, default="10.10.1.1:5000")
    args = parser.parse_args()

    client = FASClient(cid=args.cid, total_clients=args.total_clients)
    fl.client.start_client(server_address=args.server_address, client=client)
