#include <stdio.h>
#include <strings.h>
#include <unistd.h>
#include <stdlib.h>

#include <sys/socket.h>
#include <netdb.h>

#include <caml/mlvalues.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/memory.h>
#include <caml/fail.h>
#include <caml/callback.h>

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/crypto.h>

#define Ctx_val(v) (*((SSL_CTX**)Data_custom_val(v)))
#define Ssl_val(v) (*((SSL**)Data_custom_val(v)))

static void shutdown_connection(value ssl_context) {
    const SSL * ssl = Ssl_val(ssl_context);
    close(SSL_get_fd(ssl));
    SSL_CTX_free(SSL_get_SSL_CTX(ssl));
}

static struct custom_operations ctx_ops = {
    "ocaml_ssl_ctx",
    shutdown_connection,
    custom_compare_default,
    custom_hash_default,
    custom_serialize_default,
    custom_deserialize_default
};

CAMLprim void init(void) {
    SSL_library_init();
    SSL_load_error_strings();
}

CAMLprim value get_error_string(void) {
  char buf[256];
  ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
  return caml_copy_string(buf);
}

CAMLprim value create_context(void) {
    char buf[256];

    OpenSSL_add_all_algorithms();

    SSL_CTX * ctx = SSL_CTX_new(TLS_client_method());

    if (ctx == NULL) {
        ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
        caml_raise_with_arg(*caml_named_value("ssl_exn_context_error"), 
            caml_copy_string(buf));
    }

    value block = caml_alloc_custom(&ctx_ops, sizeof(SSL *), 0, 1);

    Ssl_val(block) = SSL_new(ctx);

    return block;
}

CAMLprim void use_certificate(value context, value cert_filename, value key_filename) {
    const char * cert_name = String_val(cert_filename);
    const char * key_name = String_val(key_filename);
    SSL * ssl = Ssl_val(context);
    char buf[256];

    if (SSL_use_certificate_file(ssl, cert_name, SSL_FILETYPE_PEM) <= 0) {
        ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
        caml_raise_with_arg(*caml_named_value("ssl_exn_certificate_error"), 
            caml_copy_string(buf));
    }
    
    if (SSL_use_PrivateKey_file(ssl, key_name, SSL_FILETYPE_PEM) <= 0) {
        ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
        caml_raise_with_arg(*caml_named_value("ssl_exn_private_key_error"), 
            caml_copy_string(buf));
    }
    
    if (!SSL_check_private_key(ssl)) {
        caml_raise_constant(*caml_named_value("ssl_exn_unmatching_keys"));
    }
}

CAMLprim void use_certificate_from_string(value context, value cert_string, value key_string) {
    const char * cert = String_val(cert_string);
    const char * key = String_val(key_string);
    SSL * ssl = Ssl_val(context);
    char buf[256];

    BIO * bio;
    X509 * certificate;
    bio = BIO_new(BIO_s_mem());
    BIO_puts(bio, cert);
    certificate = PEM_read_bio_X509(bio, NULL, NULL, NULL);

    if (SSL_use_certificate(ssl, certificate) <= 0) {
        ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
        caml_raise_with_arg(*caml_named_value("ssl_exn_certificate_error"), 
            caml_copy_string(buf));
    }

    BIO_puts(bio, key);
    EVP_PKEY * pkey  = EVP_PKEY_new();
    PEM_read_bio_PrivateKey(bio, &pkey, NULL, NULL);
    
    if (SSL_use_PrivateKey(ssl, pkey) <= 0) {
        ERR_error_string_n(ERR_get_error(), buf, sizeof(buf));
        caml_raise_with_arg(*caml_named_value("ssl_exn_private_key_error"), 
            caml_copy_string(buf));
    }
    
    if (!SSL_check_private_key(ssl)) {
        caml_raise_constant(*caml_named_value("ssl_exn_unmatching_keys"));
    }

    BIO_free(bio);
}

CAMLprim void open_connection_with_context(value ssl_context, value hostname_str, value port_num) {
    SSL * ssl = Ssl_val(ssl_context);
    const char * hostname = String_val(hostname_str);
    int port = Int_val(port_num);

    struct hostent * host;
    struct sockaddr_in addr;

    if ((host = gethostbyname(hostname)) == NULL) {
        caml_raise_with_arg(*caml_named_value("ssl_exn_socket_connection_error"), 
            caml_copy_string("host not found"));
    }

    int sd = socket(PF_INET, SOCK_STREAM, 0);
    bzero(&addr, sizeof(addr));
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = *(long*)(host->h_addr);

    if (connect(sd, (struct sockaddr*)&addr, sizeof(addr)) != 0) {
        close(sd);
        caml_raise_with_arg(*caml_named_value("ssl_exn_socket_connection_error"), 
            caml_copy_string("can't connect"));
    }

    SSL_set_fd(ssl, sd);
    int ret = SSL_connect(ssl);
    int err = SSL_get_error(ssl, ret);

    if (err != SSL_ERROR_NONE)
        caml_raise_with_arg(*caml_named_value("ssl_exn_connection_error"), Val_int(err));
}

CAMLprim void output_string(value ssl_context, value str) {
    SSL * ssl = Ssl_val(ssl_context);
    const char * output_string = String_val(str);
    
    int ret = SSL_write(ssl, output_string, strlen(output_string));
    int err = SSL_get_error(ssl, ret);

    if (err != SSL_ERROR_NONE)
        caml_raise_with_arg(*caml_named_value("ssl_exn_write_error"), Val_int(err));
}

CAMLprim value input_char(value ssl_context) {
    CAMLparam1(ssl_context);

    SSL * ssl = Ssl_val(ssl_context);
    char chr;

    int ret = SSL_read(ssl, &chr, 1);
    int err = SSL_get_error(ssl, ret);

    if (err != SSL_ERROR_NONE)
        caml_raise_with_arg(*caml_named_value("ssl_exn_read_error"), Val_int(err));

    CAMLreturn(Val_int(chr));
}
