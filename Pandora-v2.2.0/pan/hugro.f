      subroutine HUGRO
     $(RHEAB,IQSFS,IQHEA,IHEAB,N)
C
C     Rudolf Loeser, 1991 Jan 16
C---- Checks defaults for Helium abundance variation.
C     !DASH
      save
C     !DASH
      real*8 ONE, RHEAB
      integer IHEAB, IQHEA, IQSFS, LUEO, N
      logical ZA
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
      external NAUGHTD, SET1, KONSTD, MESHED, DVECOUT, ABORT, HI, BYE
C
C               RHEAB(N)
      dimension RHEAB(*)
C     !EJECT
C
      call HI ('HUGRO')
C     !BEG
      call NAUGHTD     (RHEAB,1,N,ZA)
      if(ZA) then
        call SET1      (RHEAB,N,ONE)
      else
C
        call KONSTD    (RHEAB,1,N,ONE,ZA)
C
        if((.not.ZA).and.(IQSFS.gt.0)) then
          call MESHED  ('HUGRO',1)
          write (LUEO,100)
  100     format(' ','RHEAB must =1 when option SPHERE=ON.')
          call DVECOUT (LUEO,RHEAB,N,'RHEAB')
          call ABORT
        end if
      end if
C
      if(IQHEA.gt.0) then
        if((IHEAB.lt.1).or.(IHEAB.gt.N)) then
          IHEAB = N/3+1
        end if
      end if
C     !END
      call BYE ('HUGRO')
C
      return
      end
