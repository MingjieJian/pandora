      subroutine FINN
     $(IPER,CNTRB,F,L,NOPAC)
C
C     Rudolf Loeser, 1974 Mar 28
C---- Normalizes and forms truncated percentages, for contributors
C     summaries.
C     !DASH
      save
C     !DASH
      real*8 CNTRB, F, FAC, FLIM, P
      integer I, IP, IPER, L, NOPAC
C     !DASH
      external  ZEROI, HI, BYE
      intrinsic min
C
C               CNTRB(Nopac), IPER(Nopac,Numkon)
      dimension CNTRB(*),     IPER(NOPAC,*)
C
      data FLIM /1.D-200/
      data FAC  /1.D2/
C
      call HI ('FINN')
C     !BEG
      if(F.gt.FLIM) then
C
        P = FAC/F
        do 100 I = 1,NOPAC
          IP = P*CNTRB(I)
          IPER(I,L) = min(IP,99)
  100   continue
C
      else
        call ZEROI (IPER(1,L), 1, NOPAC)
      end if
C     !END
      call BYE ('FINN')
C
      return
      end
