#ifndef PINSTRIO_H
#define PINSTRIO_H

#ifdef __cplusplus
extern "C" {
#endif

void _pinstrio_source_(void *ptr, int size, char *tag);
void _pinstrio_source_cnt_(void *ptr, int size, char *tag, int cnt);
void _pinstrio_sink_(void *ptr, int size, char *name);
void _pinstrio_constrained_sink_(void *ptr, int size, char *name, void *constr, int csize);
void _pinstrio_concrete_begin_();
void _pinstrio_concrete_end_();
void _pinstrio_start_();
void _pinstrio_stop_();
void _pinstrio_start_cnt_(int cnt);
void _pinstrio_abort_();

#ifdef __cplusplus
}
#endif

#endif
