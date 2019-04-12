      subroutine HOG
     $(MR,WRAT,RRCP,GOOD)
C
C     Rudolf Loeser, 2004 Nov 02
C---- Computes default RRCP for level 1, for some non-H ions, using
C
C     Verner et al. (1996), ApJ 465, 487.
C
C     IGNORES the value at the level edge.
C
C     !DASH
      save
C     !DASH
      real*8 E, EANG, EMAX, FE, FETH, RRCP, W, WRAT, WVL, ZERO, dummy
      integer I, INDEX, JDRP1, LUEO, MR, jummy
      logical GOOD
      character INAME*3, qummy*8
C     !COM
C---- APOLLO      as of 2006 Dec 04
      integer     MEST
      dimension   MEST(28)
      common      /APOLLO/ MEST
C     Atomic model parameter default values indicators
      equivalence (MEST(24),JDRP1)
C
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
C     !DASH
C     !EJECT
      external PINA, MESHED, DIVIDE, FERMENT, FERVENT, HI, BYE
C
C               WRAT(MR), RRCP(MR)
      dimension WRAT(*),  RRCP(*)
C
      data EANG /1.2398D4/
C
      call HI ('HOG')
C     !BEG
      GOOD = .false.
C
      call PINA       (0, INAME, INDEX)
C
      if(INDEX.le.0) then
        call MESHED   ('HOG', 1)
        write (LUEO,100) INAME
  100   format(' ','INAME = ',A3,'; cannot compute default value of ',
     $             'RRCP(1) using Verner et al.')
        call PINA     (LUEO, qummy, jummy)
        goto 103
      end if
C
      call FERVENT    (INDEX, dummy, dummy, EMAX, dummy, dummy, dummy,
     $                 dummy, dummy, dummy)
C
      if(EMAX.gt.ZERO) then
        call DIVIDE   (EANG, WRAT(MR), E)
        if(E.gt.EMAX) then
          W = EANG/EMAX
          call MESHED ('HOG', 1)
          write (LUEO,101) WRAT(MR),E,W,EMAX
  101     format(' ','Error in calculation of default RRCP values ',
     $               'for level 1.'/
     $           ' ','The last WRAT value is',1PE12.5,'; E =',E12.5/
     $           ' ','WRAT must not be less than ',E12.5,
     $               ', with E =',E12.5)
          goto 103
        end if
      end if
C
      WVL = ZERO
      call FERMENT    (INDEX, WVL, dummy, FETH)
      do 102 I = 1,MR
        call FERMENT  (INDEX, WRAT(I), dummy, FE)
        call DIVIDE   (FE, FETH, RRCP(I))
        JDRP1 = JDRP1+1
  102 continue
      GOOD = .true.
C
  103 continue
C     !END
      call BYE ('HOG')
C
      return
      end
