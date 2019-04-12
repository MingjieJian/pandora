      subroutine RUMBA
     $(ITER,MN1,KAMB,XN1O,XNPO,XN1N,F,G,R,S,DEE,DELTA,XN,ADDR,ADDS)
C
C     Rudolf Loeser, 1989 Sep 19
C---- Prints N1-recalculation results, for CARROT.
C     (This is version 2 of RUMBA.)
C     !DASH
      save
C     !DASH
      real*8 ADDR, ADDS, DEE, DELTA, DIFF, F, G, R, S, XN, XN1N, XN1O,
     $       XNPO
      integer I, ITER, J, KAMB, LIM, LUEO, MN1
      character CD*15, CN*15, CO*15, TIT*9
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external LINER, DASHER, MERULA, NOTICE, BRUMA, PRIVET, MARBU,
     $         SHIM, HI, BYE
C
C               XN1O(N), XNPO(N), XN1N(N), F(N), G(N), DEE(4,5,N), R(N),
      dimension XN1O(*), XNPO(*), XN1N(*), F(*), G(*), DEE(*),     R(*),
C
C               XN(N), DELTA(7,N), S(N), ADDR(N), ADDS(N)
     $          XN(*), DELTA(7,*), S(*), ADDR(*), ADDS(*)
C
      dimension TIT(3)
C
      data      TIT /' Hydrogen', ' Helium-I', 'Helium-II'/
C
      call HI ('RUMBA')
C     !BEG
      call LINER    (4, LUEO)
      call DASHER   (LUEO)
      write (LUEO,100) TIT(KAMB),ITER
  100 format(' ','Conclusion of output for:'//
     $       ' ','{N1,NK,ND}-recalculation for ',A9,
     $           '; N1-iter =',I3,'.')
      call LINER    (2, LUEO)
C
      write (LUEO,101)
  101 format(' ',12X,'f',14X,'g',14X,'r',14X,'s',15X,'N1-old',
     $           9X,'N1-new',7X,'old : new',6X,'rel diff')
      call LINER    (1, LUEO)
C
      do 103 I = 1,MN1
        call NOTICE (15, XN1O(I), XN1N(I), CO, CN, CD)
        call MARBU  (XN1O(I), XN1N(I), DIFF)
        write (LUEO,102) I,F(I),G(I),R(I),S(I),CO,CN,CD,DIFF
  102   format(' ',I4,1P4E15.7,3X,3A15,0PF13.8)
        call SHIM   (I, 5, LUEO)
  103 continue
C     !EJECT
      call LINER    (2, LUEO)
      call BRUMA    (DELTA, MN1, LIM)
      write (LUEO,104) (J,J=1,7)
  104 format(' ',14X,'Delta',8X,'Delta1''',8X,'Delta2''',
     $           7X,'Delta1''''',7X,'Delta2''''',
     $           6X,'Delta1''''''',6X,'Delta2'''''''/
     $       ' ',4X,7(7X,'DELTA(',I1,')'))
      call LINER    (1, LUEO)
C
      write (LUEO,105) (I,(DELTA(J,I),J=1,7),I=1,LIM)
  105 format(5(' ',I4,1P7E15.7/))
C
      if(LIM.lt.MN1) then
        call LINER  (1, LUEO)
        write (LUEO,106) (LIM+1),MN1
  106   format(' ',4X,'Values at depths ',I3,' to ',I3,' all = 0.')
      end if
C
      call LINER    (2, LUEO)
      write (LUEO,107)
  107 format(' ','"n" for s')
      call PRIVET   (LUEO, XN, MN1)
C
      if(KAMB.eq.3) then
        call LINER  (2, LUEO)
        write (LUEO,108) TIT(KAMB)
  108   format(' ','ADDR for r, ',A)
        call PRIVET (LUEO, ADDR, MN1)
C
        call LINER  (2, LUEO)
        write (LUEO,109) TIT(KAMB)
  109   format(' ','ADDS for s, ',A)
        call PRIVET (LUEO, ADDS, MN1)
      end if
C
      call MERULA   (DEE)
C     !END
      call BYE ('RUMBA')
C
      return
      end
