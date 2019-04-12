      subroutine MESQUIT
     $(NO,WAVE,R1N,N,Z,OPAC,SNU,XI,XICS,F,T)
C
C     Rudolf Loeser, 1978 Jun 30
C---- Prints, for RAJA.
C     !DASH
      save
C     !DASH
      real*8 F, OPAC, R1N, SNU, T, WAVE, XI, XICS, Z
      integer I, N, NO
C     !DASH
      external LINER, HI, BYE
C
C               Z(N), OPAC(N), SNU(N), XI(N), XICS(N), F(N)
      dimension Z(*), OPAC(*), SNU(*), XI(*), XICS(*), F(*)
C
      call HI ('MESQUIT')
C     !BEG
      if(NO.gt.0) then
        call LINER (4,NO)
        write (NO,100) WAVE,R1N
  100   format(' ',1PE23.9,' Angstroms',46X,'R =',E11.4//
     $         ' ',12X,'Z',14X,'K',14X,'S',14X,'I',14X,'E',14X,'F')
C
        call LINER (1,NO)
        write (NO,101) (I,Z(I),OPAC(I),SNU(I),XI(I),XICS(I),F(I),I=1,N)
  101   format(5(' ',I3,1P6E15.4/))
C
        call LINER (1,NO)
        write (NO,102) T
  102   format(' ',78X,'SH =',1PE11.4)
      end if
C     !END
      call BYE ('MESQUIT')
C
      return
      end
