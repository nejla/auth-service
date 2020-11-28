CREATE TABLE audit_log ( "id" SERIAL8 PRIMARY KEY
                       , "time" TIMESTAMP WITH TIME ZONE NOT NULL
                       , "event" VARCHAR NOT NULL
                       , "details" jsonb NOT NULL
                       , "source" VARCHAR NOT NULL
                       , "source_details" jsonb NOT NULL
                       , "user" uuid
                       );

CREATE INDEX audit_log_time ON audit_log("time");
CREATE INDEX audit_log_user ON audit_log("user");
