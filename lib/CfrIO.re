open Printf;

/*
 * You Are Entering The Land Of Side Effects
 */

let notify = arg => printf("%s\n%!", arg);

let notify_begin = arg => printf("%s%!", arg);

let notify_end = () => printf("\n%!");

