      subroutine CORDOBA
     $(LU,N,NL,NPQ,LRQ,XNUK,XNU,P,TE,XNE,HND,PF,SA,HK,H1,GM)
C
C     Rudolf Loeser, 2004 Dec 14
C---- Prints, for KALT.
C     !DASH
      save
C     !DASH
      real*8 GM, H1, HK, HND, P, PF, SA, TE, XNE, XNU, XNUK
      integer I, J, LRQ, LU, N, NL, NPQ
      logical PRNTZ
C     !DASH
      external ABJECT, LINER, OMAR, HI, BYE
C
C               GM(N,NL), NPQ(NL), LRQ(NL), XNU(NL), HND(N), XNE(N),
      dimension GM(*),    NPQ(*),  LRQ(*),  XNU(*),  HND(*), XNE(*),
C
C               TE(N), PF(N), SA(N), HK(N), H1(N), P(NL)
     $          TE(*), PF(*), SA(*), HK(*), H1(*), P(*)
C
      data PRNTZ /.false./
C
      call HI ('CORDOBA')
C     !BEG
      if(LU.gt.0) then
        call ABJECT (LU)
        write (LU,100)
  100   format(' ','Special calculation of LTE Hydrogen number ',
     $             'densities (option CHEXLOL).'//
     $         ' ',4X,'j',7X,'PQn',7X,'RQl',20X,'nu',7X,'P')
        call LINER  (1, LU)
        write (LU,101) (J,NPQ(J),LRQ(J),XNU(J),P(J),J=1,NL)
  101   format(' ',I5,2I10,1PE22.12,0PF10.1)
        write (LU,102) XNUK
  102   format(' ',25X,1PE22.12)
        call LINER  (2, LU)
        write (LU,103)
  103   format(' ',4X,'i',14X,'TE',14X,'NE',14X,'NH',14X,'PF',14X,'SA',
     $             14X,'HK',14X,'H1')
        call LINER  (1, LU)
        write (LU,104) (I,TE(I),XNE(I),HND(I),PF(I),SA(I),HK(I),H1(I),
     $                  I=1,N)
  104   format(5(' ',I5,1P7E16.8/))
        call LINER  (2, LU)
        write (LU,105)
  105   format(' ','GM')
        call OMAR   (LU, N, NL, GM, 'Level ', PRNTZ)
      end if
C     !END
      call BYE ('CORDOBA')
C
      return
      end
