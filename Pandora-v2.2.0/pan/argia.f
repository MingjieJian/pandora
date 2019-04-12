      subroutine ARGIA
     $(PRINTN,PRINTB,RUNION,LIMP,N,XNK,XND,BD,POPK,POPN,POPBD)
C
C     Rudolf Loeser, 1989 Jun 26
C---- Decides whether or not to print the population-ion data in
C     POPK, POPN and POPBD, by comparing them to the ion-of-the-run
C     data in XNK, XND and BD.
C     (This is version 4 of ARGIA.)
C     !DASH
      save
C     !DASH
      real*8 BD, DELTA, POPBD, POPK, POPN, XND, XNK
      integer J, KODE, LIMP, N
      logical PRINTB, PRINTN, RUNION
C     !DASH
      external ARISOD, HI, BYE
C
C               XNK(N), BD(N,NL), POPK(N), POPN(N,LIMP), POPBD(N,LIMP),
      dimension XNK(*), BD(N,*),  POPK(*), POPN(N,*),    POPBD(N,*),
C
C               XND(N,NL)
     $          XND(N,*)
C
      data DELTA /1.D-8/
C
      call HI ('ARGIA')
C     !BEG
      PRINTN = .true.
      PRINTB = .true.
      if(RUNION) then
C----   Check ionized number density
        call ARISOD   (XNK,      N, POPK,       N, DELTA, KODE)
        PRINTN = KODE.eq.0
        if(PRINTN) goto 101
C
C----   Check number densities for each level
        do 100 J = 1,LIMP
          call ARISOD (XND(1,J), N, POPN(1,J),  N, DELTA, KODE)
          PRINTN = KODE.eq.0
          if(PRINTN) goto 101
  100   continue
C
  101   continue
C----   Check departure coefficients for each level
        do 102 J = 1,LIMP
          call ARISOD (BD(1,J),  N, POPBD(1,J), N, DELTA, KODE)
          PRINTB = KODE.eq.0
          if(PRINTB) goto 103
  102   continue
C
  103   continue
      end if
C     !END
      call BYE ('ARGIA')
C
      return
      end
