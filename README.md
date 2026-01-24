# Monadic Social Network Simulation

A high-performance, concurrent simulation of a social network implemented in Haskell using Software Transactional Memory (STM) and modern functional design patterns.

## 🚀 Features

- **Concurrent Architecture**: Simulates thousands of independent user threads using `Control.Concurrent.Async`.
- **Lock-Free State Management**: Uses **STM (Software Transactional Memory)** for thread-safe state mutations without explicit locks, ensuring consistency and preventing deadlocks.
- **Custom Monad Stack (`AppM`)**: Leverages the **ReaderT Design Pattern** to manage dependency injection (`Env`) and custom capabilities (`MonadLog`, `MonadAtom`).
- **Smart User Personalities**: Users exhibit different behaviors (Introvert, Extrovert, Bot) influencing their activity rate and interactions.
- **Message Priority Queues**: Handles different message urgencies (Friend Requests vs Standard Gossip) using dedicated STM channels.
- **Trust & Reputation System**: Dynamically calculates user reputation scores based on interaction history.
- **Viral Trend Propagation**: Simulates the spread of "secrets" (gossip) through the network using probabilistic propagation.
- **JSON Configuration**: Fully configurable via `config.json`.

## 🛠️ Architecture

### Core Components

- **`Main.hs`**: Entry point that loads configuration and initializes the `AppM` runner.
- **`Run.hs`**: Orchestration layer. Spawns user threads and runs the central monitoring loop.
- **`UserBehaviour.hs`**: Defines the logic for individual user agents. Handles probabilistic decision-making (sending messages, sleeping) and mailbox processing.
- **`Types.hs`**: Centralizes domain types (`User`, `Message`, `Personality`) and STM data structures (`TQueue`, `TVar`).
- **`Env.hs`**: Defines the runtime environment and global application state.
- **`Capabilities.hs`**: Defines `mtl`-style typeclasses (`MonadAtom`, `MonadLog`) to abstract over side effects, enabling better testing and flexibility.
- **`Reputation.hs`**: Pure functions for calculating trust scores.

### Key Concepts

- **STM**: We use `TVar` for shared variables (like message counters) and `TQueue` for message passing channels. This allows atomic transactions across multiple state variables.
- **AppM Monad**: `type AppM = ReaderT Env IO`. This stack simplifies passing configuration and global state to all functions.

## ⚡ Quick Start

1.  **Build the project**:

    ```bash
    stack build
    ```

2.  **Run the simulation**:

    ```bash
    stack run
    ```

    _Modify `config.json` to tune the simulation parameters (number of messages, delays, etc)._

3.  **Run Tests**:
    ```bash
    stack test
    ```
