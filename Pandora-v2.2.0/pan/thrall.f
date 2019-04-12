      subroutine THRALL
     $(N,TE,CHI,CPR,XNE,HND,RAB,NL,HNI,XNP,KODE,X1,XD)
C
C     Rudolf Loeser, 1973 Mar 12
C---- Sets up defaults for NP And HN1.
C     (This is version 2 of THRALL.)
C     !DASH
      save
C     !DASH
      real*8 AB, CHI, CPR, HND, HNI, ONE, RAB, TE, X1, XD, XNE, XNP,
     $       ZERO, dummy
      integer I, INDX, KODE, N, NL
      logical STOP
C     !COM
C---- DLIT        as of 1999 Jan 29
      real*8      DLIT, ORD
      dimension   DLIT(19), ORD(11)
      equivalence (DLIT(1), ORD(1))
      common      /DLIT/ DLIT
C     Floating point literals.
      equivalence (DLIT( 1),ZERO  )
      equivalence (DLIT( 2),ONE   )
C     !DASH
      external ARRMUL, FRANK, SAGA, FLOCK, MOVE1, DIVIDE, HI, BYE
C
C               RAB(N), XNE(N), HND(N), CHI(NMT), XNP(N), X1(N), XD(N),
      dimension RAB(*), XNE(*), HND(*), CHI(*),   XNP(*), X1(*), XD(*),
C
C               CPR(N,NMT), TE(N), HNI(N,LIMP)
     $          CPR(N,*),   TE(*), HNI(N,*)
C
      data STOP /.true./
C     !EJECT
C
      call HI ('THRALL')
C     !BEG
C---- Check whether NP exists
      KODE = 1
      if(XNP(1).lt.ZERO) then
        KODE = 0
      end if
C
      if((KODE.eq.0).or.(NL.le.0)) then
C----   Get atomic data
        call FRANK    ('H  ', 0, AB, dummy, dummy, dummy, INDX)
        if(INDX.eq.0) then
          call SAGA   ('H  ', 'THRALL', STOP)
        end if
C
C----   Compute ionized fraction
        do 100 I = 1,N
          call FLOCK  (TE(I), XNE(I), CPR(I,INDX), CHI(INDX), X1(I))
          call DIVIDE ((AB*RAB(I)*HND(I)), (ONE+X1(I)), XD(I))
  100   continue
        if(KODE.eq.0) then
C----     Compute NP
          call ARRMUL (X1, XD, XNP, N)
        end if
C
C----   Set up neutral hydrogen
        if(NL.le.0) then
          call MOVE1  (XD, N, HNI(1,1))
        end if
      end if
C     !END
      call BYE ('THRALL')
C
      return
      end
