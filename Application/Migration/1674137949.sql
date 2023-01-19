CREATE TABLE users (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    "login" TEXT NOT NULL,
    email TEXT NOT NULL,
    "password" TEXT NOT NULL
);
ALTER TABLE users ADD CONSTRAINT users_login_key UNIQUE(login);
