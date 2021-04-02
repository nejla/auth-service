CREATE TABLE login_attempt
  ( "id" SERIAL8 PRIMARY KEY
  , "time" TIMESTAMP WITH TIME ZONE NOT NULL
  , "remote_address" VARCHAR NOT NULL
  , "email" VARCHAR NOT NULL
  );

CREATE INDEX login_attempt_time ON login_attempt("time");
CREATE INDEX login_attempt_user ON login_attempt("email");
