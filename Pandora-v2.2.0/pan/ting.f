      subroutine TING
     $(N,K,ICE,XI,XJNU,RXI,FRD,GRD,STZ)
C
C     Rudolf Loeser, 2006 Feb 07
C---- Saves line-center (XI=0) data for ST calculation in PERSEUS.
C     (This is version 2 of TING.)
C     !DASH
      save
C     !DASH
      real*8 FRD, GRD, RXI, STZ, XI, XJNU
      integer ICE, K, K0, N
C     !DASH
      external QUEBEC, MOVE1, HI, BYE
C
C               XJNU(N,K), RXI(N,K), FRD(N,K), GRD(N,K), STZ(N,2),
      dimension XJNU(N,*), RXI(N,*), FRD(N,*), GRD(N,*), STZ(N,*),
C
C               XI(K)
     $          XI(*)
C
      call HI ('TING')
C     !BEG
C---- Get index of value=0 in XI
      call QUEBEC  (XI, K, 'XI', 'TING', K0)
C
      if(ICE.eq.2) then
C----   For Hubeny & Lites
        call MOVE1 (FRD(1,K0),  N, STZ(1,1))
        call MOVE1 (GRD(1,K0),  N, STZ(1,2))
      else
C----   For Kneer & Heasley
        call MOVE1 (RXI(1,K0),  N, STZ(1,1))
        call MOVE1 (XJNU(1,K0), N, STZ(1,2))
      end if
C     !END
      call BYE ('TING')
C
      return
      end
