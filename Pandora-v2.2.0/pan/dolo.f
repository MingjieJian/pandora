      subroutine DOLO
     $(WSN1D,WEIT,N,XN1O,XN1,XNKO,XNK,DUMP)
C
C     Rudolf Loeser, 2001 May 23
C---- Weighting of Special-N1 & -NK (diffusion).
C     (This is version 2 of DOLO.)
C     !DASH
      save
C     !DASH
      real*8 ONE, WEIT, WSN1D, XN1, XN1O, XNK, XNKO, dummy
      integer KLOG, KMSS, LUEO, MODE, N
      logical DUMP
      character qummy*8
C     !COM
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
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external LINER, VECOUT, WEITER, HI, BYE
C
C               XN1O(N), XN1(N), XNKO(N), XNK(N), WEIT(N)
      dimension XN1O(*), XN1(*), XNKO(*), XNK(*), WEIT(*)
C
      data KLOG,MODE,KMSS /1, 0, 0/
C     !EJECT
C
      call HI ('DOLO')
C     !BEG
      if(DUMP) then
        call LINER    (2, LUEO)
      end if
C
      if(WSN1D.eq.ONE) then
        if(DUMP) then
          write (LUEO,100)
  100     format(' ','Computed N1 and NK are not weighted.')
        end if
      else
        if(DUMP) then
          write (LUEO,101) WSN1D
  101     format(' ','Computed N1 & NK are weighted; WSN1D =',1PE13.4)
C
          call VECOUT (LUEO, XN1, N, 'N1, unweighted')
          call VECOUT (LUEO, XNK, N, 'NK, unweighted')
        end if
C
        call WEITER   (XN1, XN1, XN1O, dummy, WSN1D, N, KLOG, MODE,
     $                 KMSS, qummy, WEIT)
        call WEITER   (XNK, XNK, XNKO, dummy, WSN1D, N, KLOG, MODE,
     $                 KMSS, qummy, WEIT)
C
        if(DUMP) then
          call VECOUT (LUEO, XN1, N, 'N1, weighted')
          call VECOUT (LUEO, XNK, N, 'NK, weighted')
        end if
      end if
C
      if(DUMP) then
        call LINER    (2, LUEO)
      end if
C     !END
      call BYE ('DOLO')
C
      return
      end
