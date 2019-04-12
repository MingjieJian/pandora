      subroutine MAY
     $(NO,X,TAU,N,IBEG,IEND,SYM,JU,JL)
C
C     Rudolf Loeser, 1982 May 05
C---- Plots log(TAU) vs. X, producing a scale of decades of TAU.
C     (This is version 2 of MAY.)
C     !DASH
      save
C     !DASH
      real*8 D, ONE, TAU, TEN, THREE, X, XD, XL, XR
      integer I, IBEG, ID, IEND, IL, IU, JBG, JL, JU, KNT, KODE, LGT, N,
     $        NH, NO, NV
      logical DESIRED, GOOD, KILROY
      character BLANK*1, SYM*1
C     !COM
C---- IMAGE       as of 1997 Aug 21
      integer     IMALEN
      parameter   (IMALEN=65535)
      character   IMAGE*(IMALEN)
      common      /IMAGE/ IMAGE
C     Character string to hold plot images constructed by the
C     K-type line printer plotting routines;
C     but used also as a general scratch character array.
C     .
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 2),ONE   )
      equivalence (DLIT( 4),THREE )
      equivalence (DLIT(11),TEN   )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  PLUSD, KINIT, KRIGIA, DAY, PAY, LAY, HAY, HALT, HI, BYE
      intrinsic max, abs
C
C               X(N), TAU(N)
      dimension X(*), TAU(*)
C
      data NV,NH /2, 117/
C     !EJECT
C
      call HI ('MAY')
C     !BEG
      KILROY = .true.
C
      IU = abs(JU)
      IL = abs(JL)
      if((JU.eq.0).and.(JL.eq.0)) then
        KODE = 3
      else if((JU.gt.0).and.(JL.gt.0)) then
        KODE = 1
      else if((JU.lt.0).and.(JL.lt.0)) then
        KODE = 2
      else
        write (MSSLIN(1),100) JU,JL
  100   format('JU =',I12,' and JL =',I12,
     $         ', which does not make sense.')
        call HALT     ('MAY', 1)
      end if
C
C---- Examine TAU
      JBG = max(IBEG,2)
      KNT = IEND-(JBG-1)
      call PLUSD      (TAU(JBG), 1, KNT, LGT)
      if(KNT.gt.LGT) then
        call HAY      (NO, IU, IL)
      else
        XL = X(IBEG)
        XR = X(IEND)
C----   Initialize image
        call KINIT    (IMAGE, XL, XR, ONE, THREE, NV, NH, BLANK,
     $                 GOOD)
        if(.not.GOOD) then
          call KRIGIA (XL, XR, ONE, THREE, NV, NH)
        end if
C----   Set up initial decade marker data
        call DAY      (TAU, IBEG, IEND, ID, D)
C     !EJECT
C----   Search for TAU-interval containing decade marker D
  101   continue
          ID = ID+1
          if(ID.le.9) then
            D = TEN*D
            I = IBEG
  102       continue
C----         Examine current interval
              DESIRED = ((TAU(I).le.D).and.(TAU(I+1).ge.D))
              if(.not.DESIRED) then
C----           Incorrect interval - try next one
                I = I+1
                if(I.lt.IEND) then
C----             There are more intervals to examine - do so
                  goto 102
                else
C----             There are no more intervals - try next decade marker
                  goto 101
                end if
              else
C----           Correct interval - now compute X corresponding to D
                XD = X(I)+(D-TAU(I))/(TAU(I+1)-TAU(I))*(X(I+1)-X(I))
C
C----           Plot this marker if possible
                call PAY (IMAGE, XD, ID, SYM, KILROY)
C----           Try next decade marker
                goto 101
              end if
          end if
C----   End search loop and print
        call LAY         (NO, IMAGE, IU, IL, KODE)
      end if
C     !END
      call BYE ('MAY')
C
      return
      end
