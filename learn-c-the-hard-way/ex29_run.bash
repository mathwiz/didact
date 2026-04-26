# Compilation
# yohanlee@Yohans-MacBook-Pro learn-c-the-hard-way % cc -c libex29.c -o libex29.o 
# yohanlee@Yohans-MacBook-Pro learn-c-the-hard-way % cc -shared -o libex29.so libex29.o

# make the loader program
# yohanlee@Yohans-MacBook-Pro learn-c-the-hard-way % cc -Wall -g -DNDEBUG ex29.c -ldl -o ex29 


# examples
./ex29 ./libex29.so print_a_message "hello there"

./ex29 ./libex29.so uppercase "hello there"

./ex29 ./libex29.so lowercase "Hello THERE"

./ex29 ./libex29.so fail_on_purpose "fail on purpose"


# too few args
./ex29 ./libex29.so fail_on_purpose 

# non-existent function
./ex29 ./libex29.so not_a_function whocares

# attempt to load .so that does not exist
./ex29 ./libex29NOT.so doesnotmatter willnotsee


