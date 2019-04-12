      subroutine TONG
     $(NO,KHED,F,N,CRIT,KODE,MODE,KMSS,LABEL,IMG,OLD,KERM,NERM,KLAB)
C
C     Rudolf Loeser, 2005 Apr 06
C---- Sets up error printouts for SALMON.
C     (This is version 4 of TONG.)
C     !DASH
      save
C     !DASH
      real*8 CRIT, F, OLD
      integer IMG, KERM, KHED, KLAB, KMSS, KODE, LUEO, MODE, N, NERM,
     $        NO, jummy
      logical ALLBAD
      character LABEL*(*)
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external ERNEST, LINER, UTTA, HI, BYE
C
C               F(N), OLD(N), IMG(N)
      dimension F(*), OLD(*), IMG(*)
C
      data ALLBAD /.false./
C
      call HI ('TONG')
C     !BEG
      if(NO.gt.0) then
        if(KHED.eq.0) then
C----     Assure header
          KHED = 1
          call ERNEST (NO, KLAB, jummy)
        end if
        if(LUEO.ne.NO) then
          call LINER  (2, NO)
          write (NO, 100)
  100     format(' ','Calculation of SLF required editing.')
          call LINER  (2, NO)
        end if
      end if
C
C---- Now produce the error printout that had been disabled
      call UTTA       (F, N, CRIT, KODE, MODE, KMSS, LABEL, IMG,
     $                 OLD, KERM, NERM, ALLBAD)
C     !END
      call BYE ('TONG')
C
      return
      end
