      subroutine VISAYAS
     $(LU)
C
C     Rudolf Loeser, 1986 Jul 08
C     Revised 1987 Dec.
C---- Initializes the random-access scratch file.
C
C---- All PANDORA scratch I/O uses the "VISAYAS subroutines" !
C
C---- Scratch I/O is "done"
C     either
C     in memory, using MEMOIR (q.v.),
C     or
C     on disk, using RAFAEL (q.v.).
C
C     Which to use depends on the input parameter ISCRS, and
C     on the size of the in-memory scratch I/O buffer.
C
C
C
C           Eight PANDORA-specific drivers are available;
C                   they are all described here.
C
C     (The listings of the other drivers themselves do not contain
C          detailed comments, but refer to VISAYAS instead.)
C
C     call VISAYAS (LU)
C                  to open the on-disk scratch file;
C
C     call MASBATE (NO)
C                  to print scratch I/O control/performance data;
C
C     call CEBU    (RECORD,LENGTH,IADRS)
C                  to "write" a record directly;
C
C     call BOHOL   (RECORD,LENGTH,IADRS)
C                  to "rewrite" a record directly;
C
C     call LEYTE   (RECORD,LENGTH,IADRS)
C                  to "read" a record directly;
C
C     call PANAY   (RNAME,RECORD,LENGTH, RCDNAM,DELTA,INDADR,INDLEN,
C                   INDLIM)
C                  to "write" a record, and save its name and address
C                  in an index;
C
C     call NEGROS  (RNAME,RECORD,LENGTH, RCDNAM,DELTA,INDADR,INDLEN)
C                  to "rewrite" a record by name, using an index
C                  constructed by PANAY;
C
C     call SAMAR   (RNAME,RECORD,LENGTH, RCDNAM,DELTA,INDADR,INDLEN,
C                   INDXR)
C                  to "read" a record by name, using an index
C                  contructed by PANAY.
C     !EJECT
C---- A "record index" can be constructed and used. Such an index
C     consists of two arrays, RCDNAM and INDADR, each of length INDLEN
C     (INDLEN .le. INDLIM). When using an index, a unique record name,
C     RNAME, must be specified for each record. PANAY will save RNAME
C     in RCDNAM(i), and will save the record's "address" in INDADR(i).
C     (See MEACPT and RARITE; an on-disk address is .gt. 0, an
C     in-memory address is .lt. 0). SAMAR and NEGROS will
C     look up RNAME in RCDNAM, and then use the corresponding item
C     in INDADR as the record's address.
C
C     Some index-specific errors can occur. PANAY may find that the
C     space allocated for RCDNAM and INDADR is too small
C     (i.e. INDLEN .gt. INDLIM), or that RNAME already is in RCDNAM
C     (i.e. RNAME is not unique). SAMAR or NEGROS may fail to
C     recognize RNAME (i.e. RNAME is not in RCDNAM). ("Index error"
C     printouts make use of subroutine BILIRAN.)
C
C     PANAY needs to search RCDNAM to determine whether RNAME is
C     unique; it assumes that the data in RCDNAM are >> UNSORTED. <<
C     (PANAY uses POCO to do this search.) SAMAR and NEGROS need
C     to search RCDNAM in order to determine the file address of
C     the record whose name is RNAME; they assume that the data in
C     RCDNAM are in >> ASCENDING SORTED ORDER! << (They use PACIJAN
C     to do these searches.) It is the user's responsibility to see
C     to it that the index is permuted into sorted order at the
C     appropriate time(s).
C
C     It is sometimes convenient to use a computed quantity as the
C     record name RNAME. The "same" RNAMEs computed in different
C     contexts, however, may fail to be identical bit-for-bit. Thus
C     when two record names have to be compared to determine whether
C     they are identical (i.e. during lookup in RCDNAM), a non-zero
C     tolerance, DELTA, can be specified: two RNAMEs will be "equal"
C     if their relative difference is less than DELTA.
C
C     Scratch I/O records need not all appear in just one index;
C     their addresses may be grouped in several indices. However,
C     PANAY, NEGROS and SAMAR will deal with only one index at a time.
C     !EJECT
C---- The various arguments in the above calls are described below:
C
C     DELTA    (R*8)   tolerance for "equality" of two record names;
C
C     IADRS    (I*4)   a record's "address";
C
C     INDADR   (I*4)   part of the "record index": an array allocated
C                      by the user, providing room for INDLIM items;
C
C     INDXR    (I*4)   index value determined by SAMAR, such that
C                      RNAME = RCDNAM(INDXR);
C
C     INDLEN   (I*4)   the current number of data entries in the
C                      "record index" (INDLEN .le. INDLIM);
C
C     INDLIM   (I*4)   the dimension of INDADR and RCDNAM;
C
C     LENGTH   (I*4)   the number of items in RECORD
C                      (for on-disk records, LENGTH must be .gt. 0,
C                      this is enforced in subroutine PANGLAO);
C
C     LU       (I*4)   logical unit number to be used for the random
C                      access scratch file;
C
C     NO       (I*4)   logical unit number for control/performance data
C                      output;
C
C     RCDNAM   (R*8)   part of the "record index": an array allocated
C                      by the user, providing room for INDLIM items;
C
C     RECORD   (R*8)   data array comprising the contents of the record;
C
C     RNAME    (R*8)   record name.
C     !EJECT
C     !DASH
      save
C     !DASH
      integer IOSTAT, LENREC, LU, LUEO, jummy
      logical GOOD
C     !COM
C---- MACTAN      as of 1998 Apr 03
      real*8      FILDAT
      dimension   FILDAT(11)
      common      /MACTAN/ FILDAT
C---- Control parameters for the PANDORA random-access scratch file.
C     .
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external SCRAP, RAOPEN, MESHED, IOFAULT, LINER, ABORT, HI, BYE
C
      call HI ('VISAYAS')
C     !BEG
      call SCRAP     (LENREC, jummy)
      call RAOPEN    (FILDAT, LU, LENREC, GOOD, IOSTAT)
C
      if(.not.GOOD) then
        call MESHED  ('VISAYAS', 1)
        call IOFAULT (LUEO, 'VISAYAS', 'Open', LU, IOSTAT)
        call LINER   (3, LUEO)
        write (LUEO,100) LENREC
  100   format(' ','LENREC =',I10)
        call ABORT
      end if
C     !END
      call BYE ('VISAYAS')
C
      return
      end
