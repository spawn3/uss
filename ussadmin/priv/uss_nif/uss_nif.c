#include <stdio.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "erl_nif.h"

static int
load(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
reload(ErlNifEnv* env, void** priv, ERL_NIF_TERM load_info)
{
    return 0;
}

static int
upgrade(ErlNifEnv* env, void** priv, void** old_priv,
          ERL_NIF_TERM load_info)
{
    return 0;
}

static void
unload(ErlNifEnv* env, void* priv)
{
    return;
}

static ERL_NIF_TERM hello(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        return enif_make_string(env, "Hello, world!", ERL_NIF_LATIN1);
}

/**
 * @retval 1 can get lock
 * @retval 0 cannot get lock
 * @retval -1 error
 */
static ERL_NIF_TERM trylock(ErlNifEnv *env, int argc, const ERL_NIF_TERM argv[])
{
        int ret, fd, size = 256;
        char buf[256];

        if (argc != 1)
                goto err_ret;

        ret = enif_get_string(env, argv[0], buf, size, ERL_NIF_LATIN1);
        if (ret > 0) {
                fd = open(buf, O_RDONLY);
                if (fd == -1)
                        goto err_ret;

                ret = flock(fd, LOCK_EX|LOCK_NB);
                flock(fd, LOCK_UN);
                close(fd);

                if (ret == 0)
                        return enif_make_int(env, 1);
                if (ret == -1 && errno == EWOULDBLOCK)
                        return enif_make_int(env, 0);
                else
                        goto err_open;
        } else {
                goto err_ret;
        }

        return enif_make_int(env, 0);
err_open:
        close(fd);
err_ret:
        return enif_make_int(env, -1);
}

static ErlNifFunc nif_funcs[] =
{
        {"hello", 0, hello},
        {"trylock", 1, trylock}
};

ERL_NIF_INIT(uss_nif, nif_funcs, load, reload, upgrade, unload)
