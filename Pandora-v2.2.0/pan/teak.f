      subroutine TEAK
     $(N,ITER,KAMB,XN1O,XNKO,XN1N,XN1F,XNKF,RS,RSP)
C     Rudolf Loeser, 1990 Jan 02
C---- Prints, for CARAMBA.
C     (This is version 5 of TEAK.)
C     !DASH
      save
C     !DASH
      real*8 RS, RSP, XN1F, XN1N, XN1O, XNKF, XNKO
      integer I, ITER, KAMB, LUEO, N
      character BLANK*1, CRS*15, CRSP*15
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- SYMBS       as of 1999 Feb 09
      character   SYMBS*1, ALPHS*1, NUMBS*1
      dimension   SYMBS(52),ALPHS(26),NUMBS(10)
      equivalence (SYMBS(1),ALPHS(1)),(SYMBS(27),NUMBS(1))
      common      /SYMBS/ SYMBS
C     Character literals.
      equivalence (SYMBS(43),BLANK )
C     !DASH
      external  LINER, SHIM, HI, BYE
      intrinsic max
C
C               XN1O(N), XNKO(N), XN1N(N), XN1F(N), XNKF(N), RSP(N),
      dimension XN1O(*), XNKO(*), XN1N(*), XN1F(*), XNKF(*), RSP(*),
C
C               RS(N)
     $          RS(*)
C     !EJECT
C
      call HI ('TEAK')
C     !BEG
      if(KAMB.eq.1) then
        CRS  = BLANK
        CRSP = BLANK
      else
        CRS  = '              R'
        CRSP = '        R-prime'
      end if
C
      call LINER  (4, LUEO)
      write (LUEO,101) ITER,CRS,CRSP
  101 format(' ','Final normalization for iteration ',I3//
     $       ' ',4X,A15,9X,'N1-old',9X,'N1-new',7X,'N1-final',
     $           5X,A15,9X,'NK-old',            7X,'NK-final')
      call LINER  (1, LUEO)
C
      do 104 I = 1,N
        if(KAMB.ne.1) then
          write (CRS ,102) RS(I)
          write (CRSP,102) RSP(I)
  102     format(1PE15.7)
        end if
        write (LUEO,103) I,CRS, XN1O(I),XN1N(I),XN1F(I),
     $                     CRSP,XNKO(I),        XNKF(I)
  103   format(' ',I4,A15,1P3E15.7,5X,A15,2E15.7)
        call SHIM (I, 5, LUEO)
  104 continue
C     !END
      call BYE ('TEAK')
C
      return
      end
