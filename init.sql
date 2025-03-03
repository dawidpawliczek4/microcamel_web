-- init.sql
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT NOT NULL,
    email TEXT NOT NULL,
    created_at DATETIME DEFAULT CURRENT_TIMESTAMP
);

-- Some initial data
INSERT INTO users (username, email) VALUES 
    ('test', 'test@test.com'),
    ('test', 'test@test.com');