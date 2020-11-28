ALTER TABLE "user" DROP CONSTRAINT "unique_user_email";
CREATE UNIQUE INDEX "unique_user_email" on "user" (LOWER(email));
