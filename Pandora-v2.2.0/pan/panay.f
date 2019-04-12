      subroutine PANAY
     $(RNAME,RECORD,LENGTH, RCDNAM,DELTA,INDADR,INDLEN, INDLIM)
C
C     Rudolf Loeser, 1986 Jul 08
C---- "Writes" a scratch I/O record, and saves its name and address
C     in an index.
C     (See detailed remarks in "VISAYAS".)
C     (This is version 2 of PANAY.)
C
C     RCDNAM need not be in sorted order.
C     !DASH
      save
C     !DASH
      real*8 DELTA, RCDNAM, RECORD, RNAME
      integer IADRS, INDADR, INDLEN, INDLIM, KIND, LENGTH, LUEO
C     !COM
C---- LUNITS      as of 2002 Mar 18
      integer     LUNITS
      dimension   LUNITS(37)
      common      /LUNITS/ LUNITS
C     Logical unit names.
      equivalence (LUNITS( 6),LUEO )
C     !DASH
      external PANGLAO, POCO, CEBU, BILIRAN, ABORT, LINER, MESHED,
     $         HI, BYE
C
C               RECORD(Length), RCDNAM(Indlim), INDADR(Indlim)
      dimension RECORD(*),      RCDNAM(*),      INDADR(*)
C
      call HI ('PANAY')
C     !BEG
      call PANGLAO    (LENGTH,'PANAY')
      call POCO       (RNAME,DELTA,RCDNAM,INDLEN,KIND)
C
      if((KIND.eq.0).and.(INDLEN.lt.INDLIM)) then
        call CEBU     (RECORD,LENGTH,IADRS)
        INDLEN         = INDLEN+1
        RCDNAM(INDLEN) = RNAME
        INDADR(INDLEN) = IADRS
      else
        call MESHED   ('PANAY',1)
        call BILIRAN  (LUEO,RCDNAM,INDADR,INDLEN,INDLIM,RNAME,DELTA)
        call LINER    (2,LUEO)
        if(KIND.ne.0) then
          write (LUEO,100) RNAME
  100     format(' ','RNAME =',1PE24.16,', which is not unique.')
        else
          write (LUEO,101)
  101     format(' ','Random Access file records index is full.')
        end if
        call ABORT
      end if
C     !END
      call BYE ('PANAY')
C
      return
      end
