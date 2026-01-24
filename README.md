# Social Network Simulation: Individual Coursework Report

A concurrent, high-performance social network simulation written in Haskell.
This project demonstrates advanced Functional Programming concepts including **STM (Software Transactional Memory)**, **AppM Architecture (ReaderT Pattern)**, **SOLID Design Principles**.

## Features

### 1. Hybrid Simulation Logic

Unlike simple message passers, this simulation models a complex, dynamic world:

- **Dynamic Friendships**: Users build a friend graph dynamically based on interactions.
- **Gossip Protocol**: "Secrets" (strings) are piggybacked on messages and propagate through the network.
- **Viral Trends**: A global "Topic Trend" tracker influences user behavior. If a topic goes viral, users indiscriminately friend the sender.

### 2. Key Extensions

To demonstrate advanced STM usage and functional design:

- **Trust System**: A pure functional implementation (`Reputation.hs`) that calculates influence scores based on interaction ratings.
- **Priority Queues**: Friend requests bypass the normal queue. The `processInbox` logic uses STM's `orElse` to check the urgent channel first.
- **User Personalities**: Users are randomly assigned as Introverts, Extroverts, or Bots, each with different waiting times and probabilities of initiating chat.

### 3. Architecture & Patterns

The codebase follows **SOLID principles** using the **Dependency Inversion Principle**:

- **Capabilities Pattern**: Logic relies on abstract typeclasses (`MonadAtom`, `MonadLog`) rather than concrete IO, making the core testable.
- **ReaderT Pattern**: The application runs in a custom `AppM` monad stack carrying the read-only environment.

### 4. Technical Highlights

- **JSON Configuration**: Start-up parameters are parsed safely using `Aeson`.
- **Property-Based Testing**: `QuickCheck` is used to verify graph invariants.
- **Thread-Safe Logging**: A dedicated logger handles output without race conditions.

## Project Structure

```
app/Main.hs             # Entry point
config.json             # Runtime configuration
src/AppM.hs             # Concrete Monad Impl
src/Capabilities.hs     # Abstract Interfaces
src/Config.hs           # JSON Parsing
src/Env.hs              # Global State Definition
src/Logger.hs           # Thread-safe Logging
src/Run.hs              # Simulation Orchestration
src/Types.hs            # Data Structures
src/UserBehaviour.hs    # Core Logic
```

## How to Build & Run

### Remove the chat.log file if it exists

```bash
rm chat.log
```

### Build Project

```bash
stack build
```

### Run Simulation

```bash
stack run
```

_Note: The simulation will terminate automatically when the message limit (defined in `config.json`) is reached._
