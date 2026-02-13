# Orn Lang

*A modern low-level programming language with clear error messages and fast builds*

<p align="center">
  <img src="assets/orn.png" alt="Orn Lang Logo" width="120">
</p>
<p align="center">
  <a href="#why">Why?</a> •
  <a href="#performance-architecture">Performance</a> •
  <a href="#getting-started">Getting Started</a> •
  <a href="#usage">Usage</a>
</p>

---

## Introduction

Apart from learning how compilers work because I love understanding how things run under the hood my idea with Orn is to create a strongly typed programming language with a clean and friendly syntax. Something that feels like TypeScript but also gives you the tools to work low-level with pointers and manual memory management

I also want fast compilation and great error feedback because clear errors save time and make development smoother instead of spending hours trying to figure out some cryptic Exxxx message

Right now Orn looks more like a scripting language with an imperative style but in the future I would love to add OOP like TypeScript does. It is far from done but that is the plan

---

## Why

Many low-level languages have steep learning curves that intimidate developers coming from high-level backgrounds. Orn tries to bridge that gap by offering

* **Modern syntax** – TypeScript-style type annotations with `const` and `let` that add a new layer of safety
* **Clear error feedback** – Error messages are precise and tell you exactly what went wrong
* **Low-level control** – Direct access to memory and performance-critical operations
* **Compiled** – Runs fast instead of being interpreted like `js`, `ts` or `python`
* **Strong type guarantees** – Minimize runtime surprises with a solid type system
* **Gradual learning curve** – Start with high-level concepts and dive into low-level details as needed

---

## Performance Architecture

Orn uses a **zero-copy reference design** inspired by production compilers like Clang and Rust:
```
Source Buffer (one malloc)
    ↓
Tokens (ptr+len references)
    ↓
AST (ptr+len references)
    ↓
Semantic Analysis (ptr+len references)
    ↓
IR (Three-Address Code)
    ↓
IR Optimization (multiple passes)
    ↓
Assembly (new strings)
```

**Benefits:**
- Single source allocation, thousands fewer mallocs
- No duplicate string storage throughout pipeline
- Better memory locality and faster compilation
- References become copies only in final assembly output

Traditional compilers duplicate every identifier dozens of times. Orn references the original buffer until code generation.

---

## Quick Start

### Prerequisites

* GCC or Clang
* CMake (3.10+)
* Git

### Build

### Installation
```bash
git clone https://github.com/pabloosabaterr/Orn.git
cd Orn
mkdir build && cd build
cmake ..
cmake --build .
```

You can now run Orn on your own programs:
```bash
./orn program.orn
```

Or, for verbose compilation output:
```bash
./orn --help
```

This will show all available options and usage examples.

---

## Usage

### Example Program
```typescript
const x: int = 42;
let rate: float = 3.14;
const msg: string = "Hello, World!";
const b: bool = true; // bools and ints cannot mix

if x > 0 {
   print(msg);
}

let i: int = 0;
while i <= 10 {
    print(i);
    i++;
}

// simple add function
fn add(a: int, b: int) -> int {
    return a + b;
}

const result: int = fibonacci(10);
print(result);
```

### Error Example

Orn provides actionable error messages:

```ts
error [E2005]: cannot assign to constant (x)
  --> source.orn:2:1
   |
 2 | x = 20;
   | ^
   |
   = help: assignment to immutable value
   = note: constants cannot be modified after initialization
   = suggestion: use a mutable variable instead

error [E1001]: mismatched types (x)
  --> source.orn:2:11
   |
 2 | const x: int = "hello";
   |                ^^^^^^^
   |
   = expected `int`, found `string`
   = note: string literals cannot be assigned to int variables
   = suggestion: change variable type or cast the value
```

---

## Join Us

We welcome contributors and feedback!

* Visit the [GitHub repository](https://github.com/pabloosabaterr/Orn)
* Report issues on the [issue tracker](https://github.com/pabloosabaterr/Orn/issues)
* Join our [Discord](https://discord.gg/E8qqVC9jcf)

---

<p align="center">
  <strong>Built with ❤️ </strong>
</p>
