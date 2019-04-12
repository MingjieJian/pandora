      subroutine HORUS
     $(NO,N,TE,XNE,HN1,DEL,EPS,GP,RM,RD,G,BDHM,XNHM,CRH,RMC,RDC,KOOL)
C
C     Rudolf Loeser, 17 Jun 71
C---- Prints results, for OSIRIS.
C     !DASH
      save
C     !DASH
      real*8 BDHM, CRH, DEL, EPS, G, GP, HN1, RD, RDC, RM, RMC, TE, XNE,
     $       XNHM
      integer I, IE, IS, N, NO
      logical KOOL
C     !DASH
      external  LINER, HI, BYE
      intrinsic min
C
C               BDHM(N), XNE(N), DEL(N), EPS(N), HN1(N), RM(N), RDC(N),
      dimension BDHM(*), XNE(*), DEL(*), EPS(*), HN1(*), RM(*), RDC(*),
C
C               RD(N), G(N), XNHM(N), CRH(N), RMC(N), TE(N), GP(N)
     $          RD(*), G(*), XNHM(*), CRH(*), RMC(*), TE(*), GP(*)
C     !EJECT
C
      call HI ('HORUS')
C     !BEG
      if(NO.gt.0) then
C
        IE = 0
  100   continue
          IS = IE+1
          IE = min(IE+10,N)
C
          call LINER (3, NO)
          write (NO,101)            (I      ,I=IS,IE)
  101     format(' ','Depth',2X,10I12)
C
  102     format(' ',A6,' ',1P10E12.4)
C
          call LINER (1, NO)
          write (NO,102)   'TE    ',(TE  (I),I=IS,IE)
          write (NO,102)   'NE    ',(XNE (I),I=IS,IE)
          write (NO,102)   'N1    ',(HN1 (I),I=IS,IE)
          call LINER (1, NO)
          write (NO,102)   'DEL   ',(DEL (I),I=IS,IE)
          write (NO,102)   'EPS   ',(EPS (I),I=IS,IE)
          write (NO,102)   'GP    ',(GP  (I),I=IS,IE)
          write (NO,102)   'G     ',(G   (I),I=IS,IE)
          write (NO,102)   'RM    ',(RM  (I),I=IS,IE)
          if(KOOL) then
            write (NO,102) 'RMC   ',(RMC (I),I=IS,IE)
          end if
          write (NO,102)   'RD    ',(RD  (I),I=IS,IE)
          if(KOOL) then
            write (NO,102) 'RDC   ',(RDC (I),I=IS,IE)
          end if
          call LINER (1, NO)
          write (NO,102)   'BDHM  ',(BDHM(I),I=IS,IE)
          write (NO,102)   'NHM   ',(XNHM(I),I=IS,IE)
          if(KOOL) then
            write (NO,102) 'NCRATE',(CRH (I),I=IS,IE)
          end if
C
        if(IE.lt.N) goto 100
C
      end if
C     !END
      call BYE ('HORUS')
C
      return
      end
