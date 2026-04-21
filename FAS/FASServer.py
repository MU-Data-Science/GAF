# server_eff.py
import os
import numpy as np
import flwr as fl
import tensorflow as tf

try:
    import tenseal as ts
except ImportError:
    ts = None

from tensorflow.keras.applications import EfficientNetB0
from tensorflow.keras import layers, models


SCRAMBLE_KEY = np.uint32(0xDEADBEEF)
HE_FRACTION  = 0.10
CHUNK_SIZE   = 8192


def _key_to_seed_u64(key: np.uint32) -> int:
    return int(key) & 0xFFFFFFFFFFFFFFFF


def permute_bits_from_bytes(data: bytes, key: np.uint32) -> bytes:
    """Deterministic keyed permutation over individual bits."""
    if len(data) == 0:
        return b""

    byte_arr = np.frombuffer(data, dtype=np.uint8)
    bits = np.unpackbits(byte_arr)

    rng = np.random.default_rng(_key_to_seed_u64(key))
    perm = rng.permutation(bits.size)

    scrambled_bits = bits[perm]
    scrambled_bytes = np.packbits(scrambled_bits)

    return scrambled_bytes.tobytes()


def unpermute_bits_to_bytes(data: bytes, key: np.uint32) -> bytes:
    """Invert deterministic keyed permutation over individual bits."""
    if len(data) == 0:
        return b""

    byte_arr = np.frombuffer(data, dtype=np.uint8)
    bits = np.unpackbits(byte_arr)

    rng = np.random.default_rng(_key_to_seed_u64(key))
    perm = rng.permutation(bits.size)

    inv = np.empty_like(perm)
    inv[perm] = np.arange(perm.size)

    restored_bits = bits[inv]
    restored_bytes = np.packbits(restored_bits)

    return restored_bytes.tobytes()


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


# Determine shapes and chunking from model
model_for_shape = build_model()
initial_weights = model_for_shape.get_weights()
total_len = int(sum(w.size for w in initial_weights))
enc_len   = int(HE_FRACTION * total_len)
num_chunks = (enc_len + CHUNK_SIZE - 1) // CHUNK_SIZE


# Load/create TenSEAL context
if ts is not None:
    if os.path.exists("context.pub"):
        with open("context.pub", "rb") as f:
            context = ts.context_from(f.read())
    else:
        context = ts.context(
            ts.SCHEME_TYPE.CKKS,
            poly_modulus_degree=16384,
            coeff_mod_bit_sizes=[40, 20, 40],
        )
        context.global_scale = 2**20
        context.generate_galois_keys()
        context.generate_relin_keys()
        with open("context.sec", "wb") as f:
            f.write(context.serialize(save_secret_key=True))
        with open("context.pub", "wb") as f:
            f.write(context.serialize(save_secret_key=False))
else:
    context = None


class FASStrategy(fl.server.strategy.FedAvg):
    def __init__(self):
        super().__init__()
        self.initial_parameters = fl.common.ndarrays_to_parameters(initial_weights)

    def initialize_parameters(self, client_manager):
        return self.initial_parameters

    def aggregate_fit(self, server_round, results, failures):
        if not results:
            return None, {}

        print(f"\n[Server] Aggregating round {server_round} from {len(results)} client(s)...")

        total_examples = sum(fit_res.num_examples for _, fit_res in results)

        he_sums = [None for _ in range(num_chunks)]
        rest_sum = None
        rest_len_bytes = None

        for _, fit_res in results:
            params = fit_res.parameters
            tensors = params.tensors
            if len(tensors) != num_chunks + 1:
                raise ValueError(
                    f"Expected {num_chunks+1} tensors (HE chunks + DP part), got {len(tensors)}"
                )

            he_chunks_bytes = tensors[:num_chunks]
            rest_bytes      = tensors[num_chunks]
            weight = fit_res.num_examples / total_examples

            # HE chunks aggregation
            for j, c_bytes in enumerate(he_chunks_bytes):
                if ts is not None and context is not None:
                    enc_vec = ts.ckks_vector_from(context, c_bytes)
                    if weight != 1.0:
                        enc_vec *= weight
                    if he_sums[j] is None:
                        he_sums[j] = enc_vec
                    else:
                        he_sums[j] = he_sums[j] + enc_vec
                else:
                    vals = np.frombuffer(c_bytes, dtype=np.float32)
                    if he_sums[j] is None:
                        he_sums[j] = weight * vals
                    else:
                        he_sums[j] += weight * vals

            # Tail aggregation: inverse bitwise permutation -> float32 -> weighted sum
            if len(rest_bytes) > 0:
                if rest_len_bytes is None:
                    rest_len_bytes = len(rest_bytes)
                elif len(rest_bytes) != rest_len_bytes:
                    raise ValueError(
                        f"Tail byte-length mismatch across clients: got {len(rest_bytes)}, expected {rest_len_bytes}"
                    )

                restored_bytes = unpermute_bits_to_bytes(rest_bytes, SCRAMBLE_KEY)
                rest_vals = np.frombuffer(restored_bytes, dtype=np.float32)

                if rest_sum is None:
                    rest_sum = weight * rest_vals
                else:
                    rest_sum += weight * rest_vals

        # Serialize aggregated HE chunks
        he_agg_tensors = []
        for j in range(num_chunks):
            if he_sums[j] is None:
                he_agg_tensors.append(b"")
                continue

            if ts is not None and context is not None:
                he_agg_tensors.append(he_sums[j].serialize())
            else:
                he_np = np.asarray(he_sums[j], dtype=np.float32)
                he_agg_tensors.append(he_np.tobytes())

        # Re-scramble aggregated tail with same keyed bitwise permutation
        if rest_sum is not None:
            rest_agg = rest_sum.astype(np.float32)
            rest_bytes_out = permute_bits_from_bytes(rest_agg.tobytes(), SCRAMBLE_KEY)
        else:
            rest_bytes_out = b""

        aggregated_tensors = he_agg_tensors + [rest_bytes_out]
        new_parameters = fl.common.Parameters(
            tensors=aggregated_tensors,
            tensor_type="encrypted",
        )

        print("[Server] Aggregation complete (multi-ciphertext HE + DP tail w/ bitwise permutation scrambling).")
        return new_parameters, {}


if __name__ == "__main__":
    fl.server.start_server(
        server_address="10.10.1.1:5000",
        config=fl.server.ServerConfig(num_rounds=10),
        strategy=FASStrategy(),
    )
