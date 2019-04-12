      subroutine MASO
     $(LU,IBNVW,N,NL,WPOP,IQEND,JEDIT,JNEDP,XNE,XNKUW,XNKW,XNK,
     $ XNDUW,XNDW,XNDE,XND)
C
C     Rudolf Loeser, 2003 Jun 26
C---- Prints N-development trace for MYTU.
C     !DASH
      save
C     !DASH
      real*8 WPOP, XND, XNDE, XNDUW, XNDW, XNE, XNK, XNKUW, XNKW, dummy
      integer IBNVW, IQEND, JEDIT, JNEDP, LU, N, NL
      character LABE*15, LABK*15, LABN*15, LBKU*15, LBKW*15, LBNE*15,
     $          LBNU*15, LBNW*15, qummy*8
C     !DASH
      external LINER, MOTE, MISY, HI, BYE
C
C               XNDUW(N,NL), XNDW(N,NL), XNDE(N,NL), XND(N,NL), XNE(N),
      dimension XNDUW(N,*),  XNDW(N,*),  XNDE(N,*),  XND(N,*),  XNE(*),
C
C               XNKUW(N), XNKW(N), XNK(N)
     $          XNKUW(*), XNKW(*), XNK(*)
C
      data LABE /'   NE'/
      data LBKU,LBKW,LABK,LBNU,LBNW,LABN
     $          /'   NK(new)', '   NK(weighted)', '   NK(final)',
     $           '   ND(new)', '   ND(weighted)', '   ND(final)'/
      data LBNE /'   ND(edited)'/
C
      call HI ('MASO')
C     !BEG
      if(LU.gt.0) then
        call LINER (1, LU)
        write (LU,100)
  100   format(' ','New values of NE (electron density), NK and ND ',
     $             'were computed using BD(new).')
        call LINER (1, LU)
        write (LU,101) LABE,XNE(IBNVW),LBKU,XNKUW(IBNVW)
  101   format(' ',A15,1PE16.8)
        call MOTE  (LU, NL, 1, 0, IBNVW, N, XNDUW, LBNU, dummy, qummy,
     $              dummy, qummy)
C
        call LINER (1,LU)
        write (LU,102) WPOP
  102   format(' ','NK(new) and ND(new) were weighted with NK(old) ',
     $             'and ND(old), respectively, using WPOP =',
     $             1PE12.5,'.')
        call LINER (1, LU)
        write (LU,101) LBKW,XNKW(IBNVW)
        call MOTE  (LU, NL, 1, 0, IBNVW, N, XNDW, LBNW, dummy, qummy,
     $              dummy, qummy)
C     !EJECT
        call LINER  (1,LU)
        write (LU,103)
  103   format(' ','Values of ND(weighted), at depth indices JEDIT ',
     $             'and greater, can be edited to prevent negative ',
     $             'line source functions,'/
     $         ' ','depending on option NEDIT.')
        if(IQEND.gt.0) then
          write (LU,104) JEDIT
  104     format(' ','   Since NEDIT = on, values of ND(weighted) ',
     $               'were edited, at depth indices JEDIT =',I5,' and ',
     $               'greater, whenever'/
     $           ' ','   [ N(lower) / N(upper) ] < [ GM(lower) / ',
     $               'GM(upper) ]; (the default value of ',
     $               'JEDIT = N/2 ).')
          call MISY (LU, JNEDP)
        else
          write (LU,105)
  105     format(' ','   Since NEDIT = off, ND(edited) just = ',
     $               'ND(weighted).')
        end if
        call MOTE  (LU, NL, 1, 0, IBNVW, N, XNDE, LBNE, dummy, qummy,
     $              dummy, qummy)
C
        call LINER (1, LU)
        write (LU,106)
  106   format(' ','Finally, values of NK(weighted) and ND(edited) ',
     $             'were renormalized'/
     $         ' ','to insure that the sum of NK + ND does not ',
     $             'exceed the total material available.')
        call LINER (1, LU)
        write (LU,101) LABK,XNK(IBNVW)
        call MOTE  (LU, NL, 1, 0, IBNVW, N, XND, LABN, dummy, qummy,
     $              dummy, qummy)
      end if
C     !END
      call BYE ('MASO')
C
      return
      end
