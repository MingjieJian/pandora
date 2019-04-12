      subroutine GABON
     $(LU,KODE)
C
C     Rudolf Loeser, 1987 Oct 30
C---- Sets up an input file for use.
C     !DASH
      save
C     !DASH
      integer KODE, LU, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DATAFIL     as of 1984 Apr 19
      integer     KIWILFN
      common      /DATAFIL/ KIWILFN
C     Number of unit from which to read input statements.
C     .
C     !DASH
      external LUCK, HI, BYE
C
      call HI ('GABON')
C     !BEG
      if(KODE.eq.0) then
C----   Since file is not already open, open it now
        call LUCK (LU, LUEO)
C       (This happens only once per run, hence the "rewind".)
        rewind LU
C       Set kode to tell that file has now been opened
        KODE = 1
      end if
C
C     Set lfn from which to read the next input statement
      KIWILFN = LU
C     !END
      call BYE ('GABON')
C
      return
      end
