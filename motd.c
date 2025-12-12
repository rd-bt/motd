/*******************************************************************************
 *License GPLv3+: GNU GPL version 3 or later <http://gnu.org/licenses/gpl.html>*
 *This is free software: you are free to change and redistribute it.           *
 *******************************************************************************/
#define _GNU_SOURCE
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <netdb.h>
#include <unistd.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <stdarg.h>
#include <err.h>
#include <errno.h>
#include <time.h>
#include <math.h>
#include <limits.h>
#include <alloca.h>
#include <signal.h>
#include <getopt.h>
#include <sys/poll.h>
#include <pthread.h>
#define PROT_VERSION 210
#define PORT_DEFAULT 25565
#define BUFSIZE 8192
//motd_analyse.c
static const char *expr_findpair_bracket(const char *c,const char *endp){
	size_t lv=0;
	if(*c!='[')goto err;
	while(c<endp){
		switch(*c){
			case '[':
				++lv;
				break;
			case ']':
				--lv;
				if(!lv)return c;
				break;
			default:
				break;
		}
		++c;
	}
err:
	return NULL;
}
static const char *expr_findpair_brace(const char *c,const char *endp){
	size_t lv=0;
	if(*c!='{')goto err;
	while(c<endp){
		switch(*c){
			case '{':
				++lv;
				break;
			case '}':
				--lv;
				if(!lv)return c;
				break;
			default:
				break;
		}
		++c;
	}
err:
	return NULL;
}
static const char *expr_findpair_dmark(const char *c,const char *endp){
	for(++c;c<endp;++c){
		if(*c=='\"'&&c[-1]!='\\')
			return c;
	}
	return NULL;
}
static const char *strpbrk_cbb(const char *s,const char *endp){
	while(s<endp){
		switch(*s){
			case ',':
			case '[':
			case '{':
				return s;
		}
		++s;
	}
	return s;
}
static void *xmalloc(size_t size){
	void *r;
	if(size>=SSIZE_MAX){
		warnx("IN xmalloc(size=%zu)\n"
			"CANNOT ALLOCATE MEMORY",size);
		goto ab;
	}
	r=malloc(size);
	if(!r){
		warn("IN xmalloc(size=%zu)\n"
			"CANNOT ALLOCATE MEMORY",size);
ab:
		warnx("ABORTING");
		abort();
	}
	return r;
}
static void *xrealloc(void *old,size_t size){
	void *r;
	if(size>=SSIZE_MAX){
		warnx("IN xrealloc(old=%p,size=%zu)\n"
			"CANNOT REALLOCATE MEMORY",old,size);
		goto ab;
	}
	r=old?realloc(old,size):malloc(size);
	if(!r){
		warn("IN xrealloc(old=%p,size=%zu)\n"
			"CANNOT REALLOCATE MEMORY",old,size);
ab:
		warnx("ABORTING");
		abort();
	}
	return r;
}
char *strscan(const char *s,const char *endp,char *restrict buf){
	const char *p,*s1;
	char v;
	while(s<endp)switch(*s){
		case '\\':
			switch(s[1]){
				case '\\':
					*(buf++)='\\';
					s+=2;
					break;
				case 'a':
					*(buf++)='\a';
					s+=2;
					break;
				case 'b':
					*(buf++)='\b';
					s+=2;
					break;
				case 'c':
					*(buf++)='\0';
					s+=2;
					break;
				case 'e':
					*(buf++)='\033';
					s+=2;
					break;
				case 'f':
					*(buf++)='\f';
					s+=2;
					break;
				case 'n':
					*(buf++)='\n';
					s+=2;
					break;
				case 'r':
					*(buf++)='\r';
					s+=2;
					break;
				case 't':
					*(buf++)='\t';
					s+=2;
					break;
				case 'v':
					*(buf++)='\v';
					s+=2;
					break;
				case 'x':
					s1=s;
					p=s+=2;
					while(p<endp&&((*p>='0'&&*p<='9')
						||(*p>='a'&&*p<='f')
						||(*p>='A'&&*p<='F')
						)&&p-s<2)++p;
					if(p==s)goto fail;
					v=0;
					while(s<p){
						v<<=4;
						switch(*s){
							case '0' ... '9':
								v+=*s-'0';
								break;
							case 'a' ... 'f':
								v+=*s-'a'+10;
								break;
							case 'A' ... 'F':
								v+=*s-'A'+10;
								break;
							default:
								goto fail;
						}
						++s;
					}
					*(buf++)=v;
					break;
				default:
					s1=s;
					p=s+=1;
					while(p<endp&&*p>='0'&&*p<='7'&&p-s<3)
						++p;;
					if(p==s)goto fail;
					v=0;
					while(s<p){
						v<<=3;
						v+=*s-'0';
						++s;
					}
					*(buf++)=v;
					break;
fail:
					*(buf++)=s1[1];
					s=s1+2;
					break;
			}
			break;
		default:
			*(buf++)=*(s++);
			break;
	}
	return buf;
}
enum {
	JSON_OBJECT,
	JSON_ARRAY,
	JSON_STRING,
	JSON_BOOL,
	JSON_NUMBER,
	JSON_NULL
};
struct json_node {
	int type;
	unsigned int id_len;
	size_t len;
	char *id;
	void *data;
	struct json_node *next;
};
void jn_add(struct json_node *head,struct json_node *val){
	while(head->next)
		head=head->next;
	head->next=val;
}
void jn_addp(struct json_node **head,struct json_node *val){
	if(*head)
		jn_add(*head,val);
	else
		*head=val;
}
struct json_node *jn_find(struct json_node *p,const char *id,unsigned int id_len){
	while(p){
		if(p->id&&p->id_len==id_len&&!memcmp(p->id,id,id_len)){
			return p;
		}
		p=p->next;
	}
	return NULL;
}
void jn_setid(struct json_node *p,const char *id,unsigned int id_len){
	if(p->id)
		free(p->id);
	if(!id){
		p->id=NULL;
		p->id_len=0;
		return;
	}
	p->id=xmalloc(id_len+1);
	memcpy(p->id,id,id_len);
	p->id[id_len]=0;
	p->id_len=id_len;
}
struct json_node *jn_create(const char *id,unsigned int id_len,int type,...){
	struct json_node *jp;
	void *data;
	char *data1;
	const char *p;
	size_t datalen;
	va_list ap;
	va_start(ap,type);
	switch(type){
		case JSON_OBJECT:
			data=va_arg(ap,void *);
			datalen=1;
			break;
		case JSON_ARRAY:
			data=NULL;
			datalen=0;
			break;
		case JSON_STRING:
			p=va_arg(ap,const char *);
			datalen=va_arg(ap,size_t);
			data1=xmalloc(datalen);
			memcpy(data1,p,datalen);
			data=xmalloc(datalen+1);
			p=strscan(data1,data1+datalen,data);
			free(data1);
			datalen=p-(const char *)data;
			((char *)data)[datalen]=0;
			break;
		case JSON_BOOL:
		case JSON_NUMBER:
		case JSON_NULL:
			p=va_arg(ap,const char *);
			datalen=va_arg(ap,size_t);
			data=xmalloc(datalen+1);
			memcpy(data,p,datalen);
			((char *)data)[datalen]=0;
			break;
		default:
			goto err;
	}
	jp=xmalloc(sizeof(struct json_node));
	jp->type=type;
	jp->id=NULL;
	jn_setid(jp,id,id_len);
	jp->data=data;
	jp->len=datalen;
	jp->next=NULL;
	va_end(ap);
	return jp;
err:
	va_end(ap);
	return NULL;
}
void jn_aadd(struct json_node *arr,struct json_node *val){
	if(arr->data){
		arr->data=xrealloc(arr->data,(++arr->len)*sizeof(void *));
	}else {
		arr->data=xmalloc((++arr->len)*sizeof(void *));
		arr->len=1;
	}
	((void **)arr->data)[arr->len-1]=val;
}
static const char *scan(struct json_node **jp,const char *j,const char *endp);
static struct json_node *valueof(const char *j,const char **j2,const char *endp){
	const char *p;
	struct json_node *j0,*j1;
	if(j>=endp)
		return NULL;
	switch(*j){
		case '{':
			p=expr_findpair_brace(j,endp);
			if(!p)
				return NULL;
			j0=NULL;
			scan(&j0,j+1,p);
			*j2=p+1;
			return jn_create(NULL,0,JSON_OBJECT,j0);
		case '[':
			p=expr_findpair_bracket(j,endp);
			if(!p)
				return NULL;
			++j;
			j0=jn_create(NULL,0,JSON_ARRAY,j+1,p-j-1);
			while(j<endp&&(j1=valueof(j,&j,p))){
				if(*j==',')
					++j;
				jn_aadd(j0,j1);
			}
			*j2=p+1;
			return j0;
		case '\"':
			p=expr_findpair_dmark(j,endp);
			if(!p)
				return NULL;
			*j2=p+1;
			return jn_create(NULL,0,JSON_STRING,j+1,p-j-1);
		case '-':
		case '0' ... '9':
			p=strpbrk_cbb(j,endp);
			if(!p)
				return NULL;
			*j2=p;
			return jn_create(NULL,0,JSON_NUMBER,j,p-j);
		case 't':
		case 'f':
			p=strpbrk_cbb(j,endp);
			if(!p)
				return NULL;
			*j2=p;
			return jn_create(NULL,0,JSON_BOOL,j,p-j);
		case 'n':
			p=strpbrk_cbb(j,endp);
			if(!p)
				return NULL;
			*j2=p;
			return jn_create(NULL,0,JSON_NULL,j,p-j);
		default:
			return NULL;
	}
}
static const char *scan(struct json_node **jp,const char *j,const char *endp){
	const char *p,*id;
	struct json_node *j0;
	unsigned int id_len;
	for(;;){
		if(*j!='\"')
			return NULL;
		p=expr_findpair_dmark(j,endp);
		if(!p)
			return NULL;
		id=j+1;
		id_len=p-j-1;
		if(p+2>=endp)
			return NULL;
		j0=valueof(p+2,&j,endp);
		if(!j0)
			return NULL;
		jn_setid(j0,id,id_len);
		jn_addp(jp,j0);
		if(j>=endp)
			break;
		if(*j==','){
			if(j+1>=endp)
				return NULL;
			++j;
		}
	}
	return j;
}
struct json_node *jn_scan(const char *j,size_t len){
	struct json_node *jp;
	if(*j=='{'){
		if(len<2)
			return NULL;
		++j;
		len-=2;
	}
	jp=NULL;
	scan(&jp,j,j+len);
	return jp;
}
void jn_free(struct json_node *j){
	struct json_node *p;
	do {
		switch(j->type){
			case JSON_OBJECT:
				if(j->data)
					jn_free(j->data);
				break;
			case JSON_ARRAY:
				if(j->data){
					for(size_t i=j->len-1;!(i&(1ul<<63));--i){
						jn_free(((void **)j->data)[i]);
					}
					free(j->data);
				}
				break;
			case JSON_STRING:
			case JSON_BOOL:
			case JSON_NUMBER:
			case JSON_NULL:
				free(j->data);
				break;
		}
		if(j->id)
			free(j->id);
		p=j->next;
		free(j);
	}while((j=p));
}
void *readall(int fd,ssize_t *len){
	char *buf,*p;
	size_t bufsiz,r1;
	ssize_t r,ret=0;
	bufsiz=BUFSIZE;
	if((buf=malloc(BUFSIZE))==NULL){
		if(len)*len=-1;
		return NULL;
	}
	r1=0;
	while((r=read(fd,buf+ret,BUFSIZE-r1))>0){
		r1+=r;
		ret+=r;
		if(ret==bufsiz){
			bufsiz+=BUFSIZE;
			if((p=realloc(buf,bufsiz))==NULL){
				free(buf);
				return NULL;
			}
			buf=p;
			r1=0;
		}else
			break;
	}
	if(ret==bufsiz){
		if((p=realloc(buf,bufsiz+1))==NULL){
			free(buf);
			return NULL;
		}
	buf=p;
	}
	buf[ret]=0;
	if(len)*len=ret;
	return buf;
}
void show_version(struct json_node *jn){
	fprintf(stdout,"version");
	jn=jn_find(jn,"version",7);
	if(!jn||jn->type!=JSON_OBJECT){
notfound:
		fprintf(stdout," not found\n");
		return;
	}
	jn=jn_find(jn->data,"name",4);
	if(!jn||jn->type!=JSON_STRING){
		if(!jn)abort();
		goto notfound;
	}
	fprintf(stdout,": %s\n",(char *)jn->data);
}
void print_textobj(struct json_node *jn){
	jn=jn_find(jn->data,"text",4);
	if(!jn||jn->type!=JSON_STRING)
		return;
	fprintf(stdout,"%s",(char *)jn->data);
}
void show_description(struct json_node *jn){
	struct json_node *p,**a;
	size_t len;
	fprintf(stdout,"description");
	jn=jn_find(jn,"description",11);
	if(!jn||jn->type!=JSON_OBJECT){
		fprintf(stdout," not found\n");
		return;
	}
	p=jn_find(jn->data,"text",4);
	if(p&&p->type==JSON_STRING){
		fprintf(stdout,": %s\n",(char *)p->data);
	}else
		fprintf(stdout,"\n");
	p=jn_find(jn->data,"extra",5);
	if(p&&p->type==JSON_ARRAY){
		a=(struct json_node **)p->data;
		len=p->len;
		for(size_t i=0;i<len;++i){
			switch(a[i]->type){
				case JSON_STRING:
					fprintf(stdout,"%s",(char *)a[i]->data);
					break;
				case JSON_OBJECT:
					print_textobj(a[i]);
					break;
				default:
					break;
			}
		}
		fprintf(stdout,"\n");
	}
}
void show_players(struct json_node *jn){
	struct json_node *p,**a;
	size_t len;
//	int x;
	fprintf(stdout,"player");
	jn=jn_find(jn,"players",7);
	if(!jn||jn->type!=JSON_OBJECT){
		fprintf(stdout," not found\n");
		return;
	}
	p=jn_find(jn->data,"max",3);
	if(p&&p->type==JSON_NUMBER)
		fprintf(stdout," max: %s",(char *)p->data);
	p=jn_find(jn->data,"online",6);
	if(p&&p->type==JSON_NUMBER)
		fprintf(stdout," online: %s",(char *)p->data);
	fprintf(stdout,"\n");
	p=jn_find(jn->data,"sample",6);
	if(p&&p->type==JSON_ARRAY){
		a=(struct json_node **)p->data;
		len=p->len;
		for(size_t i=0;i<len;++i){
			switch(a[i]->type){
				case JSON_STRING:
					fprintf(stdout,"%s\n",(char *)a[i]->data);
					break;
				case JSON_OBJECT:
					//x=0;
					p=jn_find(a[i]->data,"name",4);
					if(p&&p->type==JSON_STRING){
						fprintf(stdout,"%s\n",(char *)p->data);
						//x=1;
					}
					/*p=jn_find(a[i]->data,"id",2);
					if(p&&p->type==JSON_STRING){
						fprintf(stdout,",%s",(char *)p->data);
						x=1;
					}
					if(x)*/
					else
						fprintf(stdout,"\n");
					break;
				default:
					break;
			}
		}
	}
}
int motd_analyse_main(int infd,char *buf,ssize_t sz){
	struct json_node *jn;
	if(!buf){
		buf=readall(STDIN_FILENO,&sz);
		if(!buf)
			err(EXIT_FAILURE,"cannot read json");
	}
	jn=jn_scan(buf,sz);
	free(buf);
	if(jn){
		show_version(jn);
		show_description(jn);
		show_players(jn);
		jn_free(jn);
		return EXIT_SUCCESS;
	}else
		return EXIT_FAILURE;
}
//motd_analyse.c end
const struct addrinfo ai_req_tcp[1]={{
	.ai_family=AF_UNSPEC,
	.ai_socktype=SOCK_STREAM,
}};
#define printvali(x) fprintf(stderr,#x ":%d\n",x)
//#define SOCKET_BLOCK
int socket_aiopen(const struct addrinfo *ai,int timeout){
	int fd,flags;
	struct pollfd pf;
	fd=socket(ai->ai_family,ai->ai_socktype,ai->ai_protocol);
	if(fd<0)
		return -1;
	flags=fcntl(fd,F_GETFL,NULL);
	if(flags<0){
to_error:
		close(fd);
		return -2;
	}
#ifndef SOCKET_BLOCK
	if(fcntl(fd,F_SETFL,flags|O_NONBLOCK)<0)
		goto to_error;
	if(connect(fd,ai->ai_addr,ai->ai_addrlen)){
		pf.fd=fd;
		pf.events=POLLOUT;
		switch(poll(&pf,1,timeout)){
			case 1:
				if(pf.revents&POLLERR){
					write(fd,"\0",1);
					close(fd);
					return -3;
				}
				break;
			case 0:
				errno=ETIMEDOUT;
			default:
				close(fd);
				return -3;
		}
	}
#else
	if(connect(fd,ai->ai_addr,ai->ai_addrlen)<0){
		close(fd);
		return -3;
	}
#endif
	return fd;
}
ssize_t socket_read(int fd,void *buf,size_t len,int timeout){
	struct pollfd pf;
	pf.fd=fd;
	pf.events=POLLIN;
	switch(poll(&pf,1,timeout)){
		case 1:
			return read(fd,buf,len);
		case 0:
			errno=ETIMEDOUT;
		default:
			return -1;
	}
}
int tcp_open3(const char *host,struct addrinfo *aip,int timeout){
	const char *port=strrchr(host,':');
	struct addrinfo *ai0,*ai;
	char *h;
	ptrdiff_t pd;
	int fd;
	if(!port){
		port="";
	}else {
		h=alloca((pd=port-host)+1);
		h[pd]=0;
		memcpy(h,host,pd);
		host=h;
		++port;
	}
	if(getaddrinfo(host,port,ai_req_tcp,&ai0)<0)
		return -1;
	for(ai=ai0;ai;ai=ai->ai_next){
		fd=socket_aiopen(ai,timeout);
		if(fd>=0){
			if(aip){
				memcpy(aip,ai,sizeof(struct addrinfo));
				aip->ai_next=NULL;
			}
			freeaddrinfo(ai0);
			return fd;
		}
	}
	freeaddrinfo(ai0);
	return -2;
}
int tcp_open(const char *host,int timeout){
	return tcp_open3(host,NULL,timeout);
}
double dtime(void){
	struct timespec ts;
	clock_gettime(CLOCK_REALTIME,&ts);
	return (double)ts.tv_sec+ts.tv_nsec/1000000000.0;
}
double d_sleep(double x){
	struct timespec rts,ts;
	double fx;
	x=fabs(x);
	fx=floor(x);
	ts.tv_sec=(time_t)fx;
	ts.tv_nsec=(time_t)((x-fx)*1000000000.0);
	memset(&rts,0,sizeof(struct timespec));
	nanosleep(&ts,&rts);
	return (double)rts.tv_sec+rts.tv_nsec/1000000000.0;
}
int read_varint(int fd,int timeout,size_t *sz){
	int val,r=0;
	unsigned int n=0;
	do {
		if(socket_read(fd,&val,1,timeout)<1)
			return -1;
		if(sz)
			++(*sz);
		r|=(val&0x7f)<<(n*7);
		++n;
		if(n>5)
			return -2;
	}while(val&0x80);
	return r;
}
void drop(int fd,int timeout,size_t len,size_t *sz){
	static char drop_buf[BUFSIZE];
	ssize_t r;
	while(len>BUFSIZE){
		r=socket_read(fd,drop_buf,BUFSIZE,timeout);
		if(r<=0)
			return;
		if(sz)
			*sz+=r;
		len-=r;
	}
	r=socket_read(fd,drop_buf,len,timeout);
	if(r<=0)
		return;
	if(sz)
		*sz+=r;
}
ssize_t motd_read(int fd,int timeout,char **buf,size_t *bufsz){
	size_t sz=0;
	ssize_t r;
	int sz_json;
	char val;
	if(read_varint(fd,timeout,&sz)<1)
		return -1;
	if(socket_read(fd,&val,1,timeout)<1)
		return -1;
	if(val)
		return -2;
	++sz;
	sz_json=read_varint(fd,timeout,&sz);
	if(sz_json<0)
		return -3;
	if(buf){
		if(!(*buf=malloc(sz_json)))
			return -4;
		if(bufsz)
			*bufsz=sz_json;
		r=socket_read(fd,*buf,sz_json,timeout);
		if(r<=0){
			free(*buf);
			return -5;
		}
		sz+=r;
	}else
		drop(fd,timeout,sz_json,&sz);
	return sz;
}
struct request_header {
	unsigned char len,packet_id,prot_version,varint,host_len;
	char host[];
	//ushort port
	//uchar next_state
} __attribute__((packed));
void fill_request_packet(char *buf,const char *host/* without port*/,unsigned int hostlen,uint16_t port){
	((struct request_header *)buf)->len=7+hostlen;
	((struct request_header *)buf)->packet_id=0;
	((struct request_header *)buf)->prot_version=PROT_VERSION;
	((struct request_header *)buf)->varint=1;
	((struct request_header *)buf)->host_len=hostlen;
	buf=((struct request_header *)buf)->host;
	memcpy(buf,host,hostlen);
	buf+=hostlen;
	*(uint16_t *)buf=htons(port);
	buf+=2;
	*buf=1;
}
uint16_t spilt_host_port(char *host){
	char *p=strrchr(host,':');
	if(!p)
		return PORT_DEFAULT;
	*p=0;
	++p;
	return strtol(p,NULL,10);

}
struct server {
	const char *host;
	size_t req_size;
	char req[256];
	uint16_t port;
};
void server_fill(struct server *sv,const char *host0){
	ssize_t sz;
	char *host;
	uint16_t port;
	host=alloca((sz=strlen(sv->host=host0))+1);
	host[sz]=0;
	memcpy(host,host0,sz);
	port=spilt_host_port(host);
	sv->port=port;
	sz=strlen(host);
	sv->req_size=sz+8;
	fill_request_packet(sv->req,host,sz,port);
	
}
int server_request(const struct server *sv,int fd){
	if(write(fd,sv->req,sv->req_size)<(ssize_t)sv->req_size||write(fd,"\1\0",2)<2)
		return -1;
	return 0;
}
size_t minz(size_t s1,size_t s2){
	return s1>s2?s2:s1;
}
int vfdprintf_atomic(int fd,const char *restrict format,va_list ap){
	int r;
	char buf[PIPE_BUF];
//	printf("vfdprintf(%d,%s)\n",fd,format);
	if((r=vsnprintf(buf,PIPE_BUF,format,ap))==EOF)return EOF;
	return write(fd,buf,minz(r,PIPE_BUF));
}
int fprintf_atomic(FILE *restrict stream,const char *restrict format,...){
	int fd,r;
	va_list ap;
	fd=fileno(stream);
	if(fd<0)return fd;
	va_start(ap,format);
	r=vfdprintf_atomic(fd,format,ap);
	va_end(ap);
	return r;
}
int fdprintf_atomic(int fd,const char *restrict format,...){
	int r;
	va_list ap;
	va_start(ap,format);
	r=vfdprintf_atomic(fd,format,ap);
	va_end(ap);
	return r;
}
sig_atomic_t end=0;
void atsig(int sig){
	switch(sig){
		case SIGINT:
		case SIGABRT:
		case SIGALRM:
			++end;
			break;
		case SIGPIPE:
			break;
		default:
			break;
	}
}
size_t count=0;
int norecv=0,quiet=0,summary=0,data_out=0,re_resolve=0,analyse=0;
int timeout=5000;
unsigned int thread=1;
double interval=1.0;
struct echo_task {
	const struct server *sv;
	struct addrinfo ai;
	size_t sent,failed,rcvd;
	int ai_ok,tid,end;
};
#define cannot_connect ++et->failed;printt("%zu request failed - connect: %s",et->failed,strerror(errno))
#define cannot_send ++et->failed;printt("%zu request failed: %s",et->failed,strerror(errno))
#define printt(fmt,...) if(!quiet)fdprintf_atomic(STDERR_FILENO,"[%zu %.2lf ms] " fmt "\n",(dtime()-t0)*1000,et->tid,__VA_ARGS__)
void *echo(void *arg){
	int fd,r;
	ssize_t sz;
	size_t bufsz;
	double t0;
	char *buf;
	struct echo_task *et=arg;
redo:
	t0=dtime();
	if(re_resolve){
			fd=tcp_open3(et->sv->host,NULL,timeout);
			if(fd<0){
				cannot_connect;
				goto err;
			}
	}else {
		if(et->ai_ok){
			fd=socket_aiopen(&et->ai,timeout);
			if(fd<0){
				cannot_connect;
				goto err;
			}
		}else {
			fd=tcp_open3(et->sv->host,&et->ai,timeout);
			if(fd<0){
				cannot_connect;
				goto err;
			}
			et->ai_ok=1;
		}
	}
	r=server_request(et->sv,fd);
	if(norecv){
		if(!r){
			++et->sent;
			printt("%zu request sent",et->sent);
		}
		else {
			cannot_send;
		}
	}else {
		if(!r){
			sz=data_out?motd_read(fd,timeout,&buf,&bufsz):motd_read(fd,timeout,NULL,NULL);
			if(sz<0){
				++et->sent;
				printt("%zu request sent but not received: %s",et->sent,strerror(errno));
			}else {
				++et->rcvd;
				++et->sent;
				printt("%zu request sent and received %zu bytes",et->sent,sz);
				if(data_out){
					write(STDOUT_FILENO,buf,bufsz);
					free(buf);
				}
			}
		}else {
			cannot_send;
		}
	}
	close(fd);
err:
	if(!end&&(!count||et->sent+et->failed<count)){
		if(interval!=0.0)
			d_sleep(interval);
		goto redo;
	}
	et->end=1;
	return NULL;
}
int anal(struct server *sv){
	int fd,r;
	ssize_t sz;
	size_t bufsz;
	char *buf;
//	double t0;
//	t0=dtime();
	fd=tcp_open3(sv->host,NULL,timeout);
	if(fd<0){
		fdprintf_atomic(STDERR_FILENO,"request failed - connect: %s\n",strerror(errno));
		return EXIT_FAILURE;
	}
	r=server_request(sv,fd);
	if(r<0){
		fdprintf_atomic(STDERR_FILENO,"request failed: %s\n",strerror(errno));
		goto err;
	}
	sz=motd_read(fd,timeout,&buf,&bufsz);
	if(sz<0){
		fdprintf_atomic(STDERR_FILENO,"request sent but not received: %s\n",strerror(errno));
		goto err;
	}
	//fdprintf_atomic(STDERR_FILENO,"request sent and received %zu bytes in %.2lf ms\n",sz,(dtime()-t0)*1000);
	if(analyse){
		write(STDOUT_FILENO,buf,bufsz);
		write(STDERR_FILENO,"\n",1);
		free(buf);
	}else {
		motd_analyse_main(0,buf,bufsz);
	}
	close(fd);
	return EXIT_SUCCESS;
err:
	close(fd);
	return EXIT_FAILURE;
}
const struct option ops[]={
	{"analyse",0,NULL,'A'},
	{"count",1,NULL,'c'},
	{"flood",0,NULL,0xfff1},
	{"help",0,NULL,'h'},
	{"interval",1,NULL,'i'},
	{"no-recv",0,NULL,'n'},
	{"stdout",0,NULL,'C'},
	{"summary",0,NULL,'S'},
	{"thread",1,NULL,'T'},
	{"timeout",1,NULL,'t'},
	{"quiet",0,NULL,'q'},
	{"re-resolve",0,NULL,'R'},
	{NULL}
};
void show_help(const char *a0){
	fprintf(stdout,"usage: %s [options] server:port\n"
			"\t--analyse, -A\tanalyse the json returned by server\n"
			"\t--count, -c count\tcount of requests,0 for infinity,default %zu\n"
			"\t--flood\tequivalent to --interval 0 --no-recv --summary --quiet\n"
			"\t--help, -h\tshow this help\n"
			"\t--interval, -i interval(s)\tinterval between two requests,default %lg\n"
			"\t--no-recv, -n\t do not receive the packet from server\n"
			"\t--stdout, -C\twrite the json returned by server to stdout\n"
			"\t--thread, -T thread_amount\tdefault %u\n"
			"\t--timeout, -t timeout(ms)\ttimeout for connection and reading socket,default %d\n"
			"\t--quiet, -q\tdo not print to stderr when sending each packet\n"
			"\t--summary, -S\tshow a summary message\n"
			"\t--re-resolve, -R\treuse getaddrinfo() for each connection\n"
			,a0,count,interval,thread,timeout);
	exit(EXIT_SUCCESS);
}
long atol2(const char *str){
	long r;
	char *c;
	r=strtol(str,&c,10);
	if(c==str||*c)
		errx(EXIT_FAILURE,"invaild integer: %s",str);
	return r;
}
double atod2(const char *str){
	double r;
	char *c;
	r=strtod(str,&c);
	if(c==str||*c)
		errx(EXIT_FAILURE,"invaild double: %s",str);
	return r;
}
void do_summary(struct echo_task *ets,pthread_t *pts,double t0,int waitstop){
	struct echo_task *etp;
	size_t total,sent,failed,rcvd;
	sent=0;
	failed=0;
	rcvd=0;
	for(unsigned int i=0;i<thread;++i){
		if(waitstop){
			pthread_join(pts[i],NULL);
		}
		etp=ets+i;
		sent+=etp->sent;
		failed+=etp->failed;
		rcvd+=etp->rcvd;
	}
	if(waitstop&&quiet&&!summary)
		return;
	t0=dtime()-t0;
	total=sent+failed;
	fdprintf_atomic(STDERR_FILENO,"\n%zu requests sent, %zu(%.2lf%%) received, %zu(%.2lf%%) failed in %.2lf s\n",sent,rcvd,failed,sent?rcvd*100.0/sent:0.0,total?failed*100.0/total:0.0,t0);
}
int main(int argc,char **argv){
	char *host;
	struct server sv;
	struct echo_task *ets,*etp;
	double t0;
	pthread_t *pts;
	if(argc<2)
		show_help(argv[0]);
	opterr=1;
	for(;;){
		switch(getopt_long(argc,argv,"nhi:t:T:qSc:CAR",ops,NULL)){
			case 'n':
				norecv=1;
				break;
			case 'h':
				show_help(argv[0]);
			case 'A':
				++analyse;
				break;
			case 'i':
				interval=atod2(optarg);
				if(interval<0.0)
					errx(EXIT_FAILURE,"\"%s\" is not a non-negative number.",optarg);
				break;
			case 't':
				timeout=atol2(optarg);
				break;
			case 'T':
				thread=atol2(optarg);
				break;
			case 'q':
				quiet=1;
				break;
			case 'S':
				summary=1;
				break;
			case 0xfff1:
				norecv=1;
				interval=0.0;
				quiet=1;
				summary=1;
				break;
			case 'c':
				count=atol2(optarg);
				break;
			case 'C':
				data_out=1;
				break;
			case 'R':
				re_resolve=1;
				break;
			case -1:
				goto break2;
			case '?':
				exit(EXIT_FAILURE);
				break;
		}
	}
break2:
	if(optind!=argc-1){
		if(analyse)
			motd_analyse_main(STDIN_FILENO,NULL,0);
		errx(EXIT_FAILURE,"no server found");
	}
	host=argv[optind];
	server_fill(&sv,host);
	if(analyse){
		--analyse;
		return anal(&sv);
	}
	signal(SIGINT,atsig);
	signal(SIGPIPE,atsig);
	t0=dtime();
	ets=xmalloc(thread*sizeof(struct echo_task));
	pts=xmalloc(thread*sizeof(pthread_t));
	for(unsigned int i=0;i<thread;++i){
		etp=ets+i;
		etp->sv=&sv;
		etp->sent=0;
		etp->failed=0;
		etp->rcvd=0;
		etp->ai_ok=0;
		etp->end=0;
		etp->tid=i;
		pthread_create(pts+i,NULL,echo,ets+i);
	}
	if(summary){
		while(!end){
			do_summary(ets,pts,t0,0);
			sleep(1);
		}
	}
	do_summary(ets,pts,t0,1);
	free(ets);
	free(pts);
	return EXIT_SUCCESS;
}
