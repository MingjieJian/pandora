      subroutine MEMOIR
C
C     Rudolf Loeser, 1987 Dec 02
C
C---- The "MEMOIR scratch I/O" routines 'do' scratch I/O
C     into and out of a large buffer in execution memory.
C     This is feasible on systems that provide a large
C     (multi-Megabytes) "program address space".
C     ('Doing' scratch I/O in execution memory as opposed to
C     secondary memory (e.g. disk) should be very advantageous on
C     systems with large physical memories. It also may be
C     advantageous on systems with demand-paged virtual memories,
C     depending on the relative efficiencies of page-faulting
C     vs. "Fortran" I/O.)
C
C     (The MEMOIR subroutines package parallels the subroutines
C     package RAFAEL -- q.v.)
C
C---- The MEMOIR subroutines process logical records of arbitrary
C     lengths. "Records" are arrays of real*8 operands, and their
C     lengths are specified in "words" (8-bytes units).
C
C---- A sequence of records is accepted until the first record that
C     comes along that requires more buffer space than is left --
C     whereupon the run will be stopped.
C
C---- The MEMOIR subroutines associate a "buffer address" with each
C     logical record. A buffer address is assigned to a logical record
C     when it is first accepted; it must be saved by the user because
C     that record can only be returned, or updated in place, if its
C     correct buffer address is specified. A buffer address is an
C     operand of type integer*4; however, the user should not assume
C     anything about the bits it contains.
C     !EJECT
C---- The following subroutines are available:
C
C---- MEMOIR
C     "opens" the buffer; (MEMOIR must be called before any of the
C     following routines are called).
C
C---- MEACPT  (RECORD, LENGTH, IADDRS)
C     "accepts" the logical record that is LENGTH words long --
C     PROVIDED there is enough space left in the buffer (see above) --
C     and returns IADDRS, its buffer address.
C
C---- MERTRN  (RECORD, LENGTH, IADDRS)
C     "returns" the logical RECORD that is LENGTH words long and whose
C     buffer address is IADDRS. The caller must make sure that LENGTH
C     has the correct value for the record desired.
C
C---- MEUPDA  (RECORD, LENGTH, IADDRS)
C     "updates" the buffer, beginning at buffer address IADDRS, with the
C     contents of the RECORD that is LENGTH words long. The caller must
C     make sure that LENGTH does not exceed the length of the logical
C     record to which IADDRS was first assigned by MEACPT.
C
C---- MEFULL
C     "closes" the buffer (i.e. sets MELEFT equal to 0), so that no
C     more (not any) records will be accepted.
C
C---- MESHOW  (NO)
C     writes an annotated listing (i.e. a series of ASCII lines) of the
C     buffer control parameters and buffer activity counts, to the file
C     connected to logical unit number NO (at NO's current position).
C     If the value of NO is .le. 0, then MESHOW does nothing.
C
C---- MESPAC  (NW)
C     sets NW equal to the current value of MELEFT.
C
C---- MEKNTS  (NRA, NRU, NRR,  XNWA, XNWU, XNWR)
C     returns the current values
C     of   MENRAC, MENRUP, MENRRE, SENWAC, SENWUP and SENWRE,
C     in   NRA,    NRU,    NRR,    XNWA,   XNWU,  and XNWR,    resp.
C     (See below for explanations of these parameters.)
C     !EJECT
C     !DASH
      save
C     !DASH
      real*8 ZERO
C     !COM
C---- CORTEX      as of 1997 Jun 16
      integer     MELEFT,MENEXT,MENRAC,MENRUP,MENRRE,MELIMT
      real*8      SIOBUF,SENWAC,SENWUP,SENWRE
C
      dimension   SIOBUF( 1 )
C     (The   r e a l   length of SIOBUF is set in PANDORA!)
C
      common      /CORTEX1/ MELEFT,MENEXT,MENRAC,MENRUP,MENRRE
      common      /CORTEX2/ SENWAC,SENWUP,SENWRE
      common      /CORTEX3/ MELIMT
      common      /CORTEX4/ SIOBUF
C
C     Control parameters for the MEMOIR subroutines:
C
C     MELIMT = length of entire buffer (SIOBUF), in words;
C     MELEFT = number of unused words left in buffer;
C     MENEXT = index of next available word in buffer;
C     MENRAC = number of logical records accepted;
C     MENRUP = number of logical records updated;
C     MENRRE = number of logical records returned;
C     SENWAC = number of words accepted;
C     SENWUP = number of words updated;
C     SENWRE = number of words returned.
C     .
C     !DASH
      data ZERO /0.D0/
C
C     !BEG
      MENEXT = 1
      MELEFT = MELIMT
      MENRAC = 0
      MENRUP = 0
      MENRRE = 0
      SENWAC = ZERO
      SENWUP = ZERO
      SENWRE = ZERO
C     !END
C
      return
      end
