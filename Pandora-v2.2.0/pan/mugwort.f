      subroutine MUGWORT
     $(ARRAY,KOPAC,N,NOPAC,ERRAY)
C
C     Rudolf Loeser, 1995 Mar 27
C---- Copies the continuum contributions array ARRAY into ERRAY
C     (= E-dited a-RRAY), and sets =0 all those contributors that
C     have KOPAC=0.
C     (This is version 2 of MUGWORT.)
C     !DASH
      save
C     !DASH
      real*8 ARRAY, ERRAY
      integer I, KOPAC, N, NOPAC
C     !DASH
      external MOVE1, ZEROD, HI, BYE
C
C               ARRAY(Nopac,N), KOPAC(Nopac), ERRAY(Nopac,N)
      dimension ARRAY(*),       KOPAC(*),     ERRAY(NOPAC,*)
C
      call HI ('MUGWORT')
C     !BEG
      call MOVE1     (ARRAY, (NOPAC*N), ERRAY)
C
      do 100 I = 1,NOPAC
        if(KOPAC(I).le.0) then
          call ZEROD (ERRAY(I,1), NOPAC, N)
        end if
  100 continue
C     !END
      call BYE ('MUGWORT')
C
      return
      end
