#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <time.h>

#include <utmpx.h>


void print_sizes() {
  struct utmpx x;
  
  printf("Utmp size: %ld\n", sizeof(x));
  printf("ut_type size (short): %ld\n", sizeof(x.ut_type));
  printf("ut_pid size (pid_t): %ld\n", sizeof(x.ut_pid));
  printf("struct timeval size: %ld\n", sizeof(x.ut_tv));
}

char *datetime(struct timeval tv) {
  return asctime(gmtime(&(tv.tv_sec)));
}
  
void print_utmp_record(struct utmpx * utmp) {
  struct timeval tv;

  tv.tv_sec = utmp->ut_tv.tv_sec;
  tv.tv_usec = utmp->ut_tv.tv_usec;

  printf("\tRecord type: ");
  switch (utmp->ut_type) {
  case EMPTY:
    printf("empty record, skipping.\n");
    break;
  case BOOT_TIME:
    printf("time of system boot.\n");
    break;
  case LOGIN_PROCESS:
    printf("session leader process for user login.\n");
    break;
  case USER_PROCESS:
    printf("normal process.\n");
    break;
  case DEAD_PROCESS:
    printf("terminated process.\n");
    break;
  case NEW_TIME:
  case OLD_TIME:
  case INIT_PROCESS:
  default:
    printf("Unknown type: %hd, skipping.\n", utmp->ut_type);
    return;
  }

  printf("\tLogin PID: %d\n", utmp->ut_pid);
  printf("\tTTY: %.*s\n", __UT_LINESIZE, utmp->ut_line);
  printf("\tTerm suffix or inittab ID: %.*s\n", 4, utmp->ut_line);
  printf("\tUsername: %.*s\n", __UT_NAMESIZE, utmp->ut_user);
  printf("\tHostname: %.*s\n", __UT_HOSTSIZE, utmp->ut_host);
  printf("\tSession ID: %d\n", utmp->ut_session);
  printf("\tRecord time: %s\n", datetime(tv));
  //printf("\Remote host IP: %s\n", datetime(utmp->ut_tv));
  

}

void print_utmp_entries(char * filename) {
  int n_records = 0;
  ssize_t nread;
  int fd;
  struct utmpx utmp;

  fd = open(filename, O_RDONLY);
  while (1) {
    nread = read(fd, &utmp, sizeof(utmp));
    if (nread == 0) {
      printf("Finished. Total records: %d\n", n_records);
      exit(0);
    }
    if (nread != sizeof(utmp)) {
      printf("Short read: %ld instead of %ld\n", nread, sizeof(utmp));
      exit(1);
    }
    n_records++;
    printf("New record (#%d):\n", n_records);
    print_utmp_record(&utmp);
  }
}

int main(int argc, char **argv) {

  if (argc != 2) {
    printf("Expected an argument: filename or --sizes\n");
    exit(1);
  }
  if (strcmp(argv[1], "--sizes") == 0)
    print_sizes();
  else
    print_utmp_entries(argv[1]);

  exit(0);
}
