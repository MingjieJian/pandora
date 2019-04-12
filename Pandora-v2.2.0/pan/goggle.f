      subroutine GOGGLE
     $(N,NL,LIMP,ELSYM,TE,XNE,CPR,CHI,ABDEL,IPOP,POP,POPK,BD,LC,MODE,
     $ BATAL)
C
C     Rudolf Loeser, 1978 Sep 21
C---- Supplies default values of Departure Coefficient.
C     (This is version 2 of GOGGLE.)
C     !DASH
      save
C     !DASH
      real*8 ABDEL, BATAL, BD, CHI, CPR, POP, POPK, TE, XNE, dummy
      integer IPOP, J, KODE, LC, LIMP, LUEO, MODE, N, NL
      character ELSYM*3
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C     !DASH
      external WOBBLE, GOBBLE, TOGGLE, WAGGLE, VECOUT, HALT, LINER,
     $         AVAR, HI, BYE
C
C               BATAL(N,NL), POP(N,LIMP), XNE(N), CPR(N,NMT), CHI(NMT),
      dimension BATAL(*),    POP(N,*),    XNE(*), CPR(*),     CHI(*),
C
C               ABDEL(N), POPK(N), BD(N,LIMP), TE(N)
     $          ABDEL(*), POPK(*), BD(N,*),    TE(N)
C     !EJECT
C
      call HI ('GOGGLE')
C     !BEG
C---- (LC will be a level index; its value will be one
C     less than the index of the lowest computed BD set.)
      LC = 0
C---- Loop over all levels
      do 101 J = 1,LIMP
C----   Check whether BD must be supplied
        call WOBBLE       (N, BD(1,J), KODE)
        if(KODE.le.0) then
C----     No - go check next set
          LC = J
        else
C
C----     Yes - choose appropriate procedure
          if(J.eq.1) then
            if(ELSYM.ne.'ZZZ') then
              call GOBBLE (ELSYM, N, BD(1,1), POP(1,1), POPK, TE, XNE,
     $                     CPR, CHI, ABDEL)
            else
              call VECOUT (LUEO, BD, N, 'BD-1')
              MSSLIN(1) = 'BD-1 is fatally bad (negatives).'
              call HALT   ('GOGGLE', 1)
            end if
          else if(J.le.NL) then
            call TOGGLE   (J, N, LIMP, BD, POP, TE, IPOP)
          else
            call WAGGLE   (J, NL, N, LIMP, BD)
          end if
        end if
C
  101 continue
C---- Edit newly-computed Departure Coefficients
      if(MODE.eq.1) then
        call AVAR         (N, LIMP, (LC+1), LIMP, dummy, BD, BATAL, 0)
      end if
C     !END
      call BYE ('GOGGLE')
C
      return
      end
