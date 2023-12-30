#include <unistd.h>
#include <ctime>

int main() {
    while (true) {
        time_t now = time(0);
        printf("%s", ctime(&now));
        sleep(1);
    }
}