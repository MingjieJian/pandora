      subroutine MIDGE
     $(NO,KODE,CPRSS,WPRSS)
C
C     Rudolf Loeser, 1984 Oct 17
C---- Prints supplementary data, for ZUNIGA.
C     !DASH
      save
C     !DASH
      real*8 CPRSS, WPRSS
      integer KODE, NO
C     !DASH
      external LINER, HI, BYE
C
      call HI ('MIDGE')
C     !BEG
      if((NO.gt.0).and.(KODE.eq.1)) then
        call LINER (2,NO)
        write (NO,100) CPRSS,WPRSS
  100   format(' ','For the NH-adjustment to give constant pressure:'/
     $         ' ','CPRESS =',1PE14.6,',  WPRESS =',0PF6.2/
     $         ' ','NOTE: The printed values of NH (total Hydrogen ',
     $             'density) were computed from the printed values of ',
     $             'total pressure.')
      end if
C     !END
      call BYE ('MIDGE')
C
      return
      end
