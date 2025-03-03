# Microcamel Web

Microcamel Web is a lightweight OCaml library for building web applications. It offers a modular and extensible framework to simplify handling HTTP requests, database interactions, session management, and dynamic content rendering.

## Key Features

- **SQLite Integration**: Easily connect and interact with SQLite databases using raw queries or mapped results.
- **Flexible Routing**: Define dynamic routes with support for parameters, static paths, and wildcards.
- **Middleware Support**: Enhance request processing with customizable middleware (e.g., logging, header verification, session handling).
- **Asynchronous Server**: Run an asynchronous HTTP server built on Lwt and Cohttp with robust error handling.
- **Session Management**: Manage sessions in memory with automatic expiration and cleanup.
- **Templating Engine**: Render dynamic content using a simple templating system that supports variables, conditionals, and loops.
- **Static File Serving**: Serve static assets from a specified directory.

## Modules Overview

### 1. Database Module (`database.ml`)
Handles SQLite database interactions.
- **Query Modes**: Use `Raw` to receive raw rows or `Mapper f` to process results.
- **Core Functions**:
  - `connect path` – Establish a connection to the database.
  - `get_db ()` – Retrieve the active database connection.
  - `query mode sql` – Execute SQL queries based on the selected mode.
  - `init_database ~db_file ~init_sql_file` – Initialize the database with SQL commands from a file.

### 2. Middleware Module (`middleware.ml`)
Implements middleware for modifying request handling.
- **Registration & Application**:
  - `register_middleware mw` – Add middleware to the global list.
  - `apply_middlewares mws final_h` – Chain middleware functions to enhance handlers.
- **Request Callback**:
  - `callback_with_routing_and_middleware req body` – Combines routing and middleware to process requests.
- **Example Middleware**:
  - Logger: Logs request methods and paths.
  - Secret Checker: Validates the `X-Secret` header.
  - Session Manager: Handles session creation and cookie management.

### 3. Router Module (`router.ml`)
Manages HTTP request routing.
- **Route Definition**: Create routes by specifying path, HTTP method, handler, and optional middleware.
- **Key Functions**:
  - `add_route` – Register new routes.
  - `parse_path` – Match incoming paths with dynamic segments.
  - `find_route` – Identify the correct route or fall back to a default 404 handler.
  - `use_static` – Serve static files from a designated directory.

### 4. Server Module (`server.ml`)
Launches and manages the asynchronous HTTP server.
- **Core Functions**:
  - `create_server ~port callback` – Initialize the server on a specified port.
  - `start_server ~port` – Start the server and handle errors like port conflicts.

### 5. Session Module (`session.ml`)
Defines session storage and management.
- **Session Store Interface**: Methods for creating, retrieving, updating, and removing sessions.
- **In-Memory Implementation**:
  - Creates sessions with defined lifetimes and automatically cleans up expired sessions.

### 6. Template Module (`template.ml`)
Provides a basic engine for dynamic template rendering.
- **Template Processing**:
  - Tokenizes and parses templates to build a syntax tree.
  - Supports rendering of text, variables, conditionals, and loops.

### 7. Static Module (`static.ml`)
Offers functionality to serve static files asynchronously from a base directory.

## Getting Started

### Prerequisites
- OCaml
- Lwt and Cohttp libraries
- SQLite3

### Installation
Clone the repository and build the project using dune build system:

```bash
git clone https://github.com/dawidpawliczek4/microcamel-web.git
cd microcamel-web
dune build
```

### Basic Usage Example

Examples are in /bin folder.

## Contributing
Contributions are welcome! Please submit issues or pull requests to help improve the project.

## License
This project is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.
