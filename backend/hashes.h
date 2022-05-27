/*
 * hashes.h
 *      Author: aagapi
 */

#ifndef BACKEND_HASHES_H_
#define BACKEND_HASHES_H_

uint32_t hash32(unsigned int x)
// Collision free, each input bit affects each output bit with ~50% probability
{
	x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = ((x >> 16) ^ x) * 0x45d9f3b;
    x = (x >> 16) ^ x;
    return x;
}

uint64_t hash_str_sdbm(char *str)
{
	uint64_t hash = 0;
	int c;

	while (c = *str++)
		hash = c + (hash << 6) + (hash << 16) - hash;

	return hash;
}

uint64_t hash_str_djb2(char *str)
{
	uint64_t hash = 5381;
	int c;

	while (c = *str++)
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */

	return hash;
}

#endif /* BACKEND_HASHES_H_ */
