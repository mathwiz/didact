#include <stdio.h>
#include <assert.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <sqlite3.h>

#define MAX_DATA 512
#define MAX_ROWS 100

struct Book {
  char title[MAX_DATA];
  char author[MAX_DATA];
  unsigned int id;
  unsigned int set;
};

struct Database {
  sqlite3 *db;
  sqlite3_stmt *stmt;
  struct Book rows[MAX_ROWS];
};

struct Connection {
  FILE *file;
  struct Database *db;
};

void die(const char *message) {
  if (errno) {
    perror(message);
  } else {
    printf("ERROR: %s\n", message);
  }
  exit(1);
}

void Book_print(struct Book *x) {
  printf("%2u: %s; %s\n", x->id, x->title, x->author);
}

void Database_load(struct Connection *conn) {
  int rc = fread(conn->db, sizeof(struct Database), 1, conn->file);
  if (rc != 1) die("Failed to load database.");
}

struct Connection *Database_open(const char *filename, char mode) {
  struct Connection *conn = malloc(sizeof(struct Connection));

  if (!conn) die("Memory error");

  conn->db = malloc(sizeof(struct Database));
  if (!conn->db) die("Memory error");

  if (mode == 'c') {
    conn->file = fopen(filename, "w");
  } else {
    conn->file = fopen(filename, "r+");
    if (conn->file) Database_load(conn);
  }

  if(!conn->file) die("Failed to open file");

  return conn;
}

void Database_close(struct Connection *conn) {
  if (conn) {
    if (conn->file) fclose(conn->file);
    if (conn->db) free(conn->db);
    free(conn);
  }
}

void Database_write(struct Connection *conn) {
  rewind(conn->file);
  
  int rc = fwrite(conn->db, sizeof(struct Database), 1, conn->file);
  if (rc != 1) die("Failed to write database.");
  rc = fflush(conn->file);
  if (rc == -1) die("Failed to flush database.");
}

void Database_create(struct Connection *conn) {
  int i = 0;

  for (i = 0; i < MAX_ROWS; i++) {
    //make a prototype
    struct Book b = {.id = i, .set = 0 };
    //add to db
    conn->db->rows[i] = b;
  }
}

void Database_set(struct Connection *conn, int id, const char *title, const char *author) {
  struct Book *book = &conn->db->rows[id];
  if (book->set) die("Already set, delete first");

  book->set = 1;
  char *res = strncpy(book->title, title, MAX_DATA - 1);

  if (!res) die("Name copy failed");
  res = strncpy(book->author, author, MAX_DATA - 1);
  if (!res) die("Author copy failed");
}

void Database_get(struct Connection *conn, unsigned int id) {
  struct Book *book = &conn->db->rows[id];
  if (book->set) {
    Book_print(book);
  } else {
    die("Id is not set");
  }
}

void Database_delete(struct Connection *conn, unsigned int id) {
  struct Book book = {.id = id, .set = 0 };
  conn->db->rows[id] = book;
}

void Database_list(struct Connection *conn) {
  int i = 0;
  struct Database *db = conn->db;

  for (i = 0; i < MAX_ROWS; i++) {
    struct Book *cur = &db->rows[i];
    if (cur->set) Book_print(cur);
  }
}

int main(int argc, char *argv[]) {
  if (argc < 3) die("USAGE: ex17 <dbfile> <action> [action params]");

  char *filename = argv[1];
  char action = argv[2][0];
  struct Connection *conn = Database_open(filename, action);
  int id = 0;

  if (argc > 3) id = atoi(argv[3]);
  if (id >= MAX_ROWS) die("Id exceeds record count");

  switch (action) {
  case 'c':
    Database_create(conn);
    Database_write(conn);
    break;
  case 'g':
    if (argc != 4) die("Need an Id to get");
    Database_get(conn, id);
    break;
  case 's':
    if (argc != 6) die("Need an Id, Title, Author to set");
    Database_set(conn, id, argv[4], argv[5]);
    Database_write(conn);
    break;
  case 'd':
    if (argc != 4) die("Need an Id to delete");
    Database_delete(conn, id);
    Database_write(conn);
    break;
  case 'l':
    Database_list(conn);
    break;
  default:
    die("Invalid action: c=create, g=get, s=set, d=delete, l=list");
  }

  Database_close(conn);
  printf("done.\n");

  return 0;
}
