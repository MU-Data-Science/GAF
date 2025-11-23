##  Installation

1. **Clone this repository**:
   ```bash
   git clone -n --depth=1 --filter=tree:0 https://github.com/MU-Data-Science/GAF.git GAF
   cd GAF
   git sparse-checkout init --cone
   git sparse-checkout set FAS
   git checkout
   cd FAS
   ```

2. **Set up and activate a Python environment**:
   ```bash
   python3 -m venv fas-env
   source fas-env/bin/activate 
   ```

3. **Install dependencies**:
   ```bash
   pip install -r requirements.txt
   ```
---

## File Structure

```
FAS/
├── client.py        
├── server.py        
├── context.sec      # (Generated) 
├── context.pub      # (Generated) 
```

---

## Running the System

### Step 1: Start the Server
```bash
python server.py
```

This starts the Flower server, initializes the model, and handles aggregation of secure updates.

---

### Step 2: Start One or More Clients (in separate terminals)

Each client simulates training on a partition of CIFAR-10.

```bash
# Start Client 0
python client.py --cid=0 --total_clients=2

# Start Client 1
python client.py --cid=1 --total_clients=2
```

You can scale this up to as many clients as needed (adjust `--total_clients` accordingly).

---

## Cleanup

If you want to regenerate contexts:
```bash
rm context.sec context.pub
```

They will be re-created by the first client that runs.
