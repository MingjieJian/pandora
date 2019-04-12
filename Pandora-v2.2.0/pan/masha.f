      subroutine MASHA
     $(N,KAMB,HND,HK,RABD,HEND,BETA,HE2K,HE1,DEE,DELTA,F,G)
C
C     Rudolf Loeser, 1989 Sep 21
C---- Computes f and g, for FETTER.
C     !DASH
      save
C     !DASH
      real*8 BETA, DEE, DELTA, F, G, HE1, HE2K, HEND, HK, HND, RABD
      integer KAMB, N
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external RAGLAN, MILFORD, LUDLOW, HALT, HI, BYE
C
C               HND(N), F(N), G(N), HK(N), DEE(4,5,N), HE1(N), RABD(N),
      dimension HND(*), F(*), G(*), HK(*), DEE(4,5,*), HE1(*), RABD(*),
C
C               HE2K(N), DELTA(7,N), BETA(N), HEND(N)
     $          HE2K(*), DELTA(7,*), BETA(*), HEND(*)
C
      call HI ('MASHA')
C     !BEG
      if(KAMB.eq.1) then
C----   Hydrogen
        call RAGLAN  (N,HND,RABD,HK,DEE,DELTA,F,G)
C
      else if(KAMB.eq.2) then
C----   Helium-I
        call MILFORD (N,HEND,BETA,HE2K,HE1,DEE,DELTA,F,G)
C
      else if(KAMB.eq.3) then
C----   Helium-II
        call LUDLOW  (N,HEND,HE2K,HE1,DEE,DELTA,F,G)
      else
C----   Huh ?
        write (MSSLIN(1),100) KAMB
  100   format('KAMB =',I12,', which is not 1, 2, or 3.')
        call HALT    ('MASHA',1)
      end if
C     !END
      call BYE ('MASHA')
C
      return
      end
