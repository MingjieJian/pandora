      subroutine POPCORN
     $(OPER,NPOP,BUFFER)
C
C     Rudolf Loeser, 1982 Jun 02
C---- Does the work of "POPIO" (q.v.).
C     !DASH
      save
C     !DASH
      real*8 BUFFER
      integer IPOP, LLNPOP, NPOP
      logical IVALID, NVALID
      character OPER*(*)
C     !COM
C---- NOTIFY      as of 2002 Mar 15
      character   MSSLIN*127
      dimension   MSSLIN(4)
      common      /NOTIFY/ MSSLIN
C     Error messages
C     .
C---- POPDATA     as of 2007 Jan 12
      integer     NPI
      parameter   (NPI=14)
C     (Remember to recompile all users when changing NPI.)
      real*8      POPMSS
      integer     LZOQ,MRTP,NPOPS,MAXPOPL,LENPBL,MRTPA,MRTPM,LIMPOP,
     $            LENT,NAMKNT,LENPOP,ICKSM,IUPOP,IBLAD,IPSWICH,KAPNO
      character   NAMES*10,TNAMES*8,POPSYM*3,KLABPI*8,NLABPI*8,BLABPI*8
      dimension   LZOQ(5), MRTP(50),
     $            LIMPOP(NPI), NAMKNT(NPI), LENPOP(NPI), IBLAD(NPI),
     $            ICKSM(NPI),  IUPOP(NPI),  NAMES(NPI),  IPSWICH(NPI),
     $            POPSYM(NPI), KAPNO(NPI),  POPMSS(NPI), TNAMES(NPI),
     $            KLABPI(NPI), NLABPI(NPI), BLABPI(NPI)
C
      common      /POPS01/ NPOPS,MAXPOPL,LENT,LENPBL,MRTPM,MRTPA,ICKSM
      common      /POPS02/ POPMSS
      common      /POPS03/ LZOQ
      common      /POPS04/ MRTP
      common      /POPS05/ LENPOP
      common      /POPS06/ LIMPOP
      common      /POPS07/ NAMES
      common      /POPS08/ TNAMES
      common      /POPS09/ NAMKNT
      common      /POPS10/ IUPOP
      common      /POPS11/ IBLAD
      common      /POPS12/ IPSWICH
      common      /POPS13/ POPSYM
      common      /POPS14/ KAPNO
      common      /POPS15/ KLABPI
      common      /POPS16/ NLABPI
      common      /POPS17/ BLABPI
C
C     Population Data Blocks parameters and data.
      equivalence (LZOQ( 1),LLNPOP)
C     !DASH
      external POPIN, POPOUT, HALT, HI, BYE
C
C               BUFFER(Lenpbl)
      dimension BUFFER(*)
C     !EJECT
C
      call HI ('POPCORN')
C     !BEG
      IPOP   = BUFFER(LLNPOP)
      IVALID = (IPOP.ge.1).and.(IPOP.le.NPOPS)
      NVALID = (NPOP.ge.1).and.(NPOP.le.NPOPS)
C
      if(OPER.eq.'WRITE') then
        if(IVALID) then
          call POPOUT   (BUFFER, LENPBL, IBLAD(IPOP))
        end if
      else if(OPER.eq.'READ') then
        if(NVALID) then
          call POPIN    (BUFFER, LENPBL, IBLAD(NPOP))
          IPOP = BUFFER(LLNPOP)
        end if
        if(NPOP.ne.IPOP) then
          goto 101
        end if
      else if(OPER.eq.'ASSURE') then
        if(NVALID.and.((.not.IVALID).or.(IPOP.ne.NPOP))) then
          call POPIN    (BUFFER, LENPBL, IBLAD(NPOP))
          IPOP = BUFFER(LLNPOP)
        end if
        if(NPOP.ne.IPOP) then
          goto 101
        end if
      else if(OPER.eq.'EXCHANGE') then
        if(NVALID.and.(IPOP.ne.NPOP)) then
          if(IVALID) then
            call POPOUT (BUFFER, LENPBL, IBLAD(IPOP))
          end if
          call POPIN    (BUFFER, LENPBL, IBLAD(NPOP))
          IPOP = BUFFER(LLNPOP)
        end if
        if(NPOP.ne.IPOP) then
          goto 101
        end if
      else
        write (MSSLIN(1),100) OPER
  100   format('OPER = ',A,', which is not WRITE, READ, ASSURE or ',
     $         'EXCHANGE.')
        call HALT       ('POPCORN', 1)
      end if
      goto 103
C
  101 continue
      write (MSSLIN(1),102) OPER,NPOP,IPOP
  102 format('OPER = ',A,': NPOP =',I12,' must equal IPOP =',I12)
      call HALT         ('POPCORN', 1)
C
  103 continue
C     !END
      call BYE ('POPCORN')
C
      return
      end
