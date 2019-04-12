      subroutine MOTA
     $(LU,IBNVW,N,NL,MBREC,JNEDP,BDI)
C
C     Rudolf Loeser, 2003 Jun 26
C---- Prints final B-results for MYTU.
C     (This is version 2 of MOTA.)
C     !DASH
      save
C     !DASH
      real*8 BDI, dummy
      integer IBNVW, JNEDP, LU, MBREC, N, NL
      character LABB*15, qummy*8
C     !DASH
      external LINER, MISY, MOTE, HI, BYE
C
C               BDI(N,NL)
      dimension BDI(N,*)
C
      data LABB /'BD(final)'/
C
      call HI ('MOTA')
C     !BEG
      if(LU.gt.0) then
        call LINER  (1, LU)
        write (LU,100)
  100   format(' ','Values of BD(final) either = BD(new) or can be ',
     $             'recomputed, depending on the switch MBREC ( = 0 ',
     $             'or 1; default = 1).')
C
        if(MBREC.gt.0) then
          write (LU,101)
  101     format(' ','   Since MBREC = 1, BD(final) were computed ',
     $               'directly from the final values of ND ',
     $               'according to'/
     $           ' ','   BDm = ( Nm / GMm ) * ( BD1 * GM1 / N1 ).')
          call MISY (LU, JNEDP)
        else
          write (LU,102)
  102     format(' ','   Since MBREC = 0, BD(final) just = BD(new).')
        end if
C
        call MOTE   (LU, NL, 1, 0, IBNVW, N, BDI, LABB, dummy, qummy,
     $               dummy, qummy)
      end if
C     !END
      call BYE ('MOTA')
C
      return
      end
