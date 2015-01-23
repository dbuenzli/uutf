v0.9.4 2015-01-23 La Forclaz (VS)
---------------------------------

- Add `Uutf.decoder_byte_count` returning the bytes decoded so far.
- The `utftrip` cli utility now uses `Cmdliner` which becomes an
  optional dependency of the package. The cli interface is not
  compatible with previous versions.

v0.9.3 2013-08-10 Cambridge (UK)
--------------------------------

- Fix wrong decoding sequence when an UTF-8 encoding guess is based on
  a two byte UTF-8 sequence. Thanks to Edwin Török for the report.
- OPAM friendly workflow and drop OASIS support.

v0.9.2 2013-01-04 La Forclaz (VS)
---------------------------------

- utftrip, better tool help.
- Fix `Uutf.is_uchar` always returning false. Thanks to Edwin Török 
  for reporting and providing the fix and test.

v0.9.1 2012-08-05 Lausanne
--------------------------

- OASIS 0.3.0 support.

v0.9.0 2012-05-05 La Forclaz (VS)
---------------------------------

First release.
