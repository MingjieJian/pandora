      subroutine TEILO
     $(KAMB,N,HND,HEND,XN1O,XNKO,XN1N,ZION,XN1F,XNKF,RS,RSP)
C
C     Rudolf Loeser, 1998 Feb 17
C---- Normalization of N1 & NK, for diffusion calculations.
C     !DASH
      save
C     !DASH
      real*8 HEND, HND, RS, RSP, XN1F, XN1N, XN1O, XNKF, XNKO, ZION
      integer I, KAMB, N
C     !DASH
      external ZERO1, KNAWEL, HI, BYE
C
C               XN1F(N), HEND(N), XN1O(N), XNKO(N), XN1N(N), ZION(N),
      dimension XN1F(*), HEND(*), XN1O(*), XNKO(*), XN1N(*), ZION(*),
C
C               HND(N), XNKF(N), RS(N), RSP(N)
     $          HND(*), XNKF(*), RS(*), RSP(*)
C
      call HI ('TEILO')
C     !BEG
      if(KAMB.eq.1) then
        do 100 I = 1,N
          if(XN1N(I).gt.HND(I)) then
            XN1F(I) = XN1O(I)
          else
            XN1F(I) = XN1N(I)
          end if
          XNKF(I) = HND(I)-XN1F(I)
  100   continue
        call ZERO1  (RS ,N)
        call ZERO1  (RSP,N)
C
      else
        call KNAWEL (N,XN1O,XNKO,XN1N,ZION,XN1F,XNKF,RS,RSP)
      end if
C     !END
      call BYE ('TEILO')
C
      return
      end
