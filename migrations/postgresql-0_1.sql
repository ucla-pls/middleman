CREATE TABLE "group" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "name" VARCHAR NOT NULL
    , "timeout" DOUBLE PRECISION NOT NULL
);

CREATE TABLE "job_description" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "derivation" VARCHAR NOT NULL
    , "group_id" INT8 NOT NULL
);

CREATE TABLE "job" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "desc_id" INT8 NOT NULL
    , "work_id" INT8 NULL
    , "output" VARCHAR NOT NULL
);

CREATE TABLE "worker" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "name" VARCHAR NOT NULL
    , "ipaddress" INT8 NOT NULL
);

CREATE TABLE "work" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "worker_id" INT8 NOT NULL
    , "job_id" INT8 NOT NULL
    , "started" TIMESTAMP WITH TIME ZONE NOT NULL
    , "result_id" INT8 NULL
);

CREATE TABLE "result" (
    "id" SERIAL8 PRIMARY KEY UNIQUE
    , "ended" TIMESTAMP WITH TIME ZONE NOT NULL
    , "success" INT4 NOT NULL
);

ALTER TABLE "group"
    ADD CONSTRAINT "unique_group_name" UNIQUE ("name");

ALTER TABLE "job_description"
    ADD CONSTRAINT "unique_job_derivation" UNIQUE ("derivation")
    , ADD CONSTRAINT "job_description_group_id_fkey" FOREIGN KEY ("group_id") REFERENCES "group" ("id") ON DELETE CASCADE;

ALTER TABLE "job"
    ADD CONSTRAINT "unique_job_description" UNIQUE ("desc_id")
    , ADD CONSTRAINT "unique_job_work_id" UNIQUE ("work_id")
    , ADD CONSTRAINT "job_desc_id_fkey" FOREIGN KEY ("desc_id") REFERENCES "job_description" ("id") ON DELETE CASCADE
    , ADD CONSTRAINT "job_work_id_fkey" FOREIGN KEY ("work_id") REFERENCES "work" ("id") ON DELETE SET NULL;

ALTER TABLE "worker"
    ADD CONSTRAINT "unique_worker_id" UNIQUE ("ipaddress");

ALTER TABLE "work"
    ADD CONSTRAINT "unique_result_id" UNIQUE ("result_id")
    , ADD CONSTRAINT "work_worker_id_fkey" FOREIGN KEY ("worker_id") REFERENCES "worker" ("id")
    , ADD CONSTRAINT "work_job_id_fkey" FOREIGN KEY ("job_id") REFERENCES "job" ("id") ON DELETE CASCADE
    , ADD CONSTRAINT "work_result_id_fkey" FOREIGN KEY ("result_id") REFERENCES "result" ("id");

