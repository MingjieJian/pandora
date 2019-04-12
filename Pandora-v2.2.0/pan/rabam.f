      subroutine RABAM
     $(N,NL,MN1, NO,KAMB,HESIM,XND,XNK,Z,HND,HEND,HE1,HEK,ALFA,RABD,
     $ XN1O,RN1,XNTI,XNNT,RRNT,ALFO,ALSM,ALED,HE1P,W,IW)
C
C     Rudolf Loeser, 1996 May 22
C---- Normalizes number densities, for diffusion.
C     !DASH
      save
C     !DASH
      real*8 ALED, ALFA, ALFO, ALSM, HE1, HE1P, HEK, HEND, HND, RABD,
     $       RN1, RRNT, W, XN1O, XND, XNK, XNNT, XNTI, Z
      integer IW, J, KAMB, MN1, N, NL, NO
      logical HESIM, PRINT, PRNTZ
      character TYPE*3
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external ARRDIV, ARRMUL, ENORM, ROWSUM, ARRADD, MOREN, NELKE,
     $         ROMA, LINER, HALT, HI, BYE
C
      dimension W(*), IW(*)
C
C               XND(N,NL), HEND(N), RABD(N), XNTI(N), XNNT(N), RRNT(N),
      dimension XND(N,*),  HEND(*), RABD(*), XNTI(*), XNNT(*), RRNT(*),
C
C               XNK(N), ALED(N), ALFA(N), HEK(N), HND(N), HE1(N), Z(N),
     $          XNK(*), ALED(*), ALFA(*), HEK(*), HND(*), HE1(*), Z(*),
C
C               XN1O(N), RN1(N), ALFO(N), ALSM(N), HE1P(N)
     $          XN1O(*), RN1(*), ALFO(*), ALSM(*), HE1P(*)
C
      data TYPE /'lin'/
      data PRNTZ /.true./
C
      call HI ('RABAM')
C     !BEG
      PRINT = NO.gt.0
      if(PRINT) then
        call LINER  (2,NO)
        write (NO,100)
  100   format(' ','::::: Begin normalization and adjustment of ',
     $             'number densities.')
      end if
C
      call ARRDIV   (XND(1,1), XN1O, RN1, N)
      if(PRINT) then
        call ENORM  (NO, N, NL, XN1O, XND(1,1), XND, RN1)
      end if
C     !EJECT
      do 101 J = 2,NL
        call ARRMUL (XND(1,J), RN1, XND(1,J), N)
  101 continue
      if(PRINT) then
        call LINER  (2, NO)
        write (NO,102)
  102   format(' ','ND(new) = RN1 * ND')
        call ROMA   (NO, N, NL, 2, NL, XND, 'Level ', PRNTZ)
      end if
C
      call ROWSUM     (XND, N, N, 1, NL, XNNT)
      call ARRADD     (XNNT, XNK, XNNT, N)
      if(KAMB.eq.1) then
        call ARRMUL   (HND , RABD, XNTI, N)
      else if((KAMB.eq.2).or.(KAMB.eq.3)) then
        call ARRMUL   (HEND, RABD, XNTI, N)
      else
        write (MSSLIN(1),103) KAMB
  103   format('KAMB =',I12,', which is not 1, 2 or 3.')
        call HALT     ('RABAM', 1)
      end if
      call ARRDIV     (XNTI, XNNT, RRNT, N)
C
      if(.not.HESIM) then
        call ARRMUL   (RRNT, XNK     , XNK     , N)
        do 104 J = 1,NL
          call ARRMUL (RRNT, XND(1,J), XND(1,J), N)
  104   continue
      end if
C
      if(PRINT) then
        call MOREN    (NO, N, NL, KAMB, HESIM, HND, HEND, RABD, XNTI,
     $                 RRNT, XNNT, XNK, XND)
      end if
C
      if(HESIM) then
C----   Do new wrinkle in case of He-II [98 Dec 18]
        call NELKE    (N, NL, MN1, XND, XNK, HE1, HEK, ALFO, ALFA, ALSM,
     $                 ALED, Z, HEND, RABD, XNNT, RRNT, HE1P, NO, W, IW)
      end if
C
      if(PRINT) then
        call LINER    (1, NO)
        write (NO,105)
  105   format(' ','::::: End of normalization and adjustment of ',
     $             'number densities.')
        call LINER    (2, NO)
      end if
C     !END
      call BYE ('RABAM')
C
      return
      end
