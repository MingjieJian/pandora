      subroutine VANUA
     $(NO,N,TE,XNE)
C
C     Rudolf Loeser, 1981 Feb 02
C---- Prints sample Coulomb cross-sections.
C     !DASH
      save
C     !DASH
      real*8 CQ11, CQ13, CQ15, CQ16, CQ21, CQ22, CQ23, CQ33, CQ41, CQ42,
     $       CQ43, CQ44, CQ45, CQ56, TE, XNE
      integer K, N, NO
      character H*4
C     !DASH
      external UZU, COULOMB, HI, BYE
C
C               TE(N), XNE(N)
      dimension TE(*), XNE(*)
C
      dimension H(6)
C
      data H /'E   ', 'H   ', 'H+  ', 'HE  ', 'HE+ ', 'HE++'/
C
      call HI ('VANUA')
C     !BEG
      if(NO.gt.0) then
C
        call UZU     (TE, N, K)
C
        call COULOMB (H(1), H(1), TE(K), XNE(K), CQ11)
        call COULOMB (H(1), H(3), TE(K), XNE(K), CQ13)
        call COULOMB (H(3), H(3), TE(K), XNE(K), CQ33)
        call COULOMB (H(1), H(5), TE(K), XNE(K), CQ15)
        call COULOMB (H(1), H(6), TE(K), XNE(K), CQ16)
        call COULOMB (H(5), H(6), TE(K), XNE(K), CQ56)
        call COULOMB (H(2), H(2), TE(K), XNE(K), CQ22)
        call COULOMB (H(2), H(3), TE(K), XNE(K), CQ23)
        call COULOMB (H(2), H(1), TE(K), XNE(K), CQ21)
        call COULOMB (H(4), H(4), TE(K), XNE(K), CQ44)
        call COULOMB (H(4), H(5), TE(K), XNE(K), CQ45)
        call COULOMB (H(4), H(1), TE(K), XNE(K), CQ41)
        call COULOMB (H(4), H(2), TE(K), XNE(K), CQ42)
        call COULOMB (H(4), H(3), TE(K), XNE(K), CQ43)
C
        write (NO,100) K,TE(K),XNE(K),
     $                 H(1),H(1),CQ11,H(1),H(3),CQ13,H(3),H(3),CQ33,
     $                 H(1),H(5),CQ15,H(1),H(6),CQ16,H(5),H(6),CQ56,
     $                 H(2),H(2),CQ22,H(2),H(3),CQ23,H(2),H(1),CQ21,
     $                 H(4),H(4),CQ44,H(4),H(5),CQ45,H(4),H(1),CQ41,
     $                 H(4),H(2),CQ42,H(4),H(3),CQ43
  100   format(  ' ','Sample values of Coulomb cross-sections at ',
     $               'depth ',I2,', TE=',1PE20.12,', XNE=',E20.12//
     $        (3(' ',10X,A4,' <-> ',A4,1PE16.8)/))
      end if
C     !END
      call BYE ('VANUA')
C
      return
      end
