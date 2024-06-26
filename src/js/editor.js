var transparent = 'transparent';
var black = '#1f2937';
var absolute = 'absolute';
var none = 'none';
var relative = 'relative';

var insert = (textarea, symbol) => {
    var s = textarea.value;
    var pos = textarea.selectionEnd;
    textarea.value = s.substring(0, pos) + symbol + s.substring(pos, s.length);
    textarea.selectionEnd = pos;
};
var auto_close = (key, textarea) => {
    switch (key) {
        case '{':
            insert(textarea, '}');
            break;
        case '[':
            insert(textarea, ']');
            break;
        case '(':
            insert(textarea, ')');
            break;
        case '"':
            insert(textarea, '"');
            break;
    }
};

var zdhl = (elid) => {
    let el = document.getElementById(elid);
    let code = el.parentElement;
    let v = el.innerText;
    code.innerHTML = v
        .replace(/:[a-zA-Z0-9][-.:\/_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:green'>${x}</b>`;
        })
        .replace(/#[-.:_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:blue'>${x}</b>`;
        })
        .replace(/\^[-_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:orange'>${x}</b>`;
        })
        .replace(/(\(\([^\)]+\)\))/gi, (x) => {
            return `<b style='color:purple'>${x}</b>`;
        })
        .replace(/(\[\[[^\]]+\]\])/gi, (x) => {
            return `<b style='color:purple'>${x}</b>`;
        });
};

var hl = (ctx, v) => {
    var t = ctx.editorContainer.els.editor.els.textarea;
    var h = ctx.editorContainer.els.editor.els.hl;
    var c = ctx.editorContainer.els.editor.els.cursor;
    set(t, { style: { opacity: '1' } });
    set(h, { style: { opacity: '0' } });
    h.innerHTML = v
        .replace(/:[a-zA-Z0-9][-.:\/_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:gold'>${x}</b>`;
        })
        .replace(/#[-.:_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:blue'>${x}</b>`;
        })
        .replace(/\^[-_a-zA-Z0-9]+/gi, (x) => {
            return `<b style='color:purple'>${x}</b>`;
        })
        //.replace(/("[^"]+")/gi, (x)=> { return `<b style='color:green'>${x}</b>`; })
        .replace(/(\(\([^\)]+\)\))/gi, (x) => {
            return `<b style='color:purple'>${x}</b>`;
        })
        .replace(/(\[\[[^\]]+\]\])/gi, (x) => {
            return `<b style='color:purple'>${x}</b>`;
        });
    set(h, { style: { opacity: '1' } });
    set(t, { style: { opacity: 0.5 } });

    setTimeout(() => {
        var rect = h.getBoundingClientRect();
        set(t, { style: { width: rect.width, height: rect.height + 500 } });
        set(c, { style: { width: rect.width, height: rect.height + 500 } });
    }, 300);
};

var keys = [':title', ':tags', ':desc', ':role'];
var options = {
    key: (ctx, token) => {
        if (token.startsWith(':fa-')) {
            return ctx.icons.search(token.substring(4)).map((x) => {
                return x.item;
            });
        } else {
            return ctx.keys.search(token).map((x) => {
                return x.item;
            });
        }
    },
    symbol: (ctx, token) => {
        console.log('token', token);
        let res = ctx.symbols.search(token).map((x) => {
            console.log(x.item);
            return x.item;
        });
        if (token == '/') {
            res.unshift({ title: '<entr>', name: '/\n' });
        } else {
            res.unshift({ name: token, icon: ['fa-solid', 'fa-plus'] });
        }
        return res;
    },
    annotation: (ctx, token) => {
        return ctx.annotations.search(token).map((x) => {
            return x.item;
        });
    },
};

var hide_popup = (ctx) => {
    var els = ctx.editorContainer.els.editor.els;
    ctx.items = null;
    set(els.pop, { style: { display: 'none' } });
};

var selection_style = { background: 'blueviolet' };

var insert_text = (ctx, text) => {
    var textarea = ctx.editorContainer.els.editor.els.textarea;
    var v = textarea.value;
    var start = textarea.selectionStart;
    var new_v = v.substring(0, ctx.insert_at) + text + v.substring(start, v.length);
    textarea.value = new_v;
    textarea.selectionEnd = ctx.insert_at + text.length;
    hl(ctx, new_v);
    hide_popup(ctx);
    textarea.dispatchEvent(new KeyboardEvent('keypress', {}));
};

var insert_item = (ctx, item) => {
    insert_text(ctx, item.name);
    // var textarea = ctx.editorContainer.els.editor.els.textarea;
    // var v = textarea.value;
    // var start = textarea.selectionStart;
    // var new_v = v.substring(0, ctx.insert_at) + item.name + v.substring(start, v.length);
    // textarea.value = new_v;
    // textarea.selectionEnd = ctx.insert_at + item.name.length;
    // hl(ctx, new_v);
    // hide_popup(ctx);
    // textarea.dispatchEvent(new KeyboardEvent("keypress", {}));
};

var insert_selection = (ctx) => {
    var item = ctx.items[ctx.selection];
    insert_item(ctx, item);
};

var autocompl = (ctx, v) => {
    var els = ctx.editorContainer.els.editor.els;
    var btxt = v.substring(0, els.textarea.selectionStart);

    // var line_start  = btxt.lastIndexOf("\n");
    // var str_start   = btxt.lastIndexOf('"');
    // var colon_start = btxt.lastIndexOf(':');
    // var ann_start = btxt.lastIndexOf('^');
    // var space_start = btxt.lastIndexOf(' ');
    // var token_start = -1;

    var c = null;
    var j = 0;
    var lst_idx = btxt.length - 1;
    var p_aln = is_alphanum(btxt[lst_idx]);
    var pos_type = null;
    var token_start = -1;

    for (var i = lst_idx; i > 0; i--) {
        j = j + 1;
        c = btxt[i];
        if (c == '^' && (p_aln || j == 1) && (i == 0 || btxt[i - 1] == '\n')) {
            pos_type = 'annotation';
            token_start = i;
            break;
        } else if (c == ':') {
            console.log('here');
            pos_type = 'key';
            token_start = i;
            break;
        } else if (j > 1 && c == '#' && p_aln) {
            pos_type = 'symbol';
            token_start = i + 1;
            break;
        } else if (j > 1 && !is_alphanum(c) && c !== '/' && p_aln && is_in_key(btxt)) {
            pos_type = 'symbol';
            token_start = i + 1;
            break;
        }
        if (c == '\n') {
            break;
        }
        if (p_aln && !is_alphanum(c)) {
            p_aln = false;
        }
    }
    if (pos_type) {
        var token = btxt.substring(token_start);
        var items = options[pos_type](ctx, token);
        if (items.length > 0) {
            items = items.slice(0, 100);
            ctx.items = items;
            ctx.selection = 0;
            ctx.insert_at = token_start;
            var atxt = v.substring(0, token_start);
            els.caret.els.txt.textContent = atxt;
            els.caret.els.cursor.innerText = token;
            var cur = els.caret.els.cursor;
            var item_els = {};
            for (let i = 0; i < items.length; i++) {
                var item = items[i];
                var icon = null;
                if (item.logo) {
                    icon = {
                        tag: 'img',
                        src: item.logo,
                        style: {
                            height: 16,
                            'border-radius': '1px',
                            'padding-right': '0.5rem',
                            display: 'inline-block',
                        },
                    };
                } else if (item.icon) {
                    icon = { tag: 'span', class: item.icon, style: { 'font-size': 12, 'padding-right': '0.5rem' } };
                }
                var opts = {
                    tag: 'div',
                    class: ['menu-item'],
                    on: {
                        click: (ev) => {
                            ctx.selection = i;
                            insert_selection(ctx);
                        },
                    },
                    els: {
                        icon: icon,
                        name: { tag: 'b', style: { 'padding-right': 5 }, text: (item.name || '').trim() },
                        title: { tag: 'span', text: item.title },
                    },
                    style: { padding: 5, 'font-size': 12, cursor: 'point' },
                };
                if (i == 0) {
                    opts.style = merge(opts.style, selection_style);
                }
                item_els[i] = opts;
            }
            set(els.pop, {
                style: {
                    display: 'block',
                    top: cur.offsetTop + 18,
                    left: cur.offsetLeft,
                    height: 500,
                    'overflow-y': 'auto',
                },
                els: item_els,
            });
        } else {
            hide_popup(ctx);
        }
    } else {
        hide_popup(ctx);
    }
};

var get_in = (obj, path) => {
    var res = obj;
    for (var i of path) {
        res = res[i];
        if (!res) {
            return res;
        }
    }
    return res;
};

var select = (ctx, dir) => {
    var textarea = ctx.editorContainer.els.editor.els.popup;
    var sel = ctx.selection;
    var len = ctx.items.length;
    if (sel == 0 && dir < 0) {
        sel = len - 1;
    } else if (sel > len - 2 && dir > 0) {
        sel = 0;
    } else if (sel < len) {
        sel = sel + dir;
    }
    set(get_in(ctx, ['editor', 'els', 'pop', 'els', ctx.selection]), { style: { background: 'transparent' } });
    set(get_in(ctx, ['editor', 'els', 'pop', 'els', sel]), { style: selection_style, intoView: true });

    ctx.selection = sel;
};

var save = (ctx) => {
    var value = sanitize(ctx.editorContainer.els.editor.els.textarea.value);
    var doc = ctx.doc;
    fetch(`/${doc}`, { method: 'POST', body: value }).then((resp) => {
        if (resp.status == 200) {
            resp.text().then((docid) => {
                window.location.href = docid;
            });
        } else {
            ctx.container.els.save.style.visibility = 'visible';
        }
    });
};

var cancel = (ctx) => {
    var doc = ctx.doc;
    window.location.href = `/${doc}`;
};

var on_editor_keydown = (ctx, ev) => {
    if (ev.ctrlKey && ev.key == 's') {
        save(ctx);
        stop(ev);
        return false;
    } else if (ev.ctrlKey && ev.key == 'x') {
        cancel(ctx);
    }
    if (ctx.items) {
        if (ev.key == 'Enter') {
            stop(ev);
            insert_selection(ctx);
            ctx.skip_up = true;
            return false;
        } else if (ev.key == 'ArrowUp' || (ev.key == 'p' && ev.ctrlKey)) {
            stop(ev);
            select(ctx, -1);
            ctx.skip_up = true;
            return false;
        } else if (ev.key == 'ArrowDown' || (ev.key == 'n' && ev.ctrlKey)) {
            stop(ev);
            select(ctx, +1);
            ctx.skip_up = true;
            return false;
        } else if (ev.key == 'c' && ev.ctrlKey) {
            hide_popup(ctx);
        }
    }
};

var sanitize = (s) => {
    return s.replace(/[\u0020\u00a0\u1680\u2000-\u200a\u2028\u2029\u202f\u205f\u3000\ufeff]/g, ' ');
};

var _render = (content) => {
    fetch(`/${ctx.doc}/preview`, { method: 'POST', body: content }).then((resp) => {
        resp.text().then((txt) => {
            ctx.preview.innerHTML = txt;
            ctx.preview.querySelectorAll('script').forEach((x) => {
                eval(x.innerText);
            });
            // reload widgets
            update_widgets();
        });
    });
};

var render = debounce(_render, 300);

var on_editor_keyup = (ctx, ev) => {
    if (!ctx.in_chrome) {
        render(sanitize(ev.target.value));
    }

    if (ctx.skip_up) {
        ctx.skip_up = false;
        return;
    }
    var t = ev.target;
    var v = t.value;
    if (v !== ctx.prev_value) {
        ctx.prev_value = v;
        //auto_close(ev.key, t);
        hl(ctx, v);
        autocompl(ctx, v);
    }
};

var poss = {
    display: 'block',
    border: none,
    outline: none,
    background: transparent,
    'font-size': 14,
    padding: 10,
    margin: 0,
    'line-height': 20,
    'overflow-wrap': 'break-word',
    'white-space': 'pre-wrap',
    'font-family': 'monospace',
};

var popup_style = {
    position: absolute,
    display: none,
    'box-shadow': '1px 2px 2px #ddd',
    'min-width': '10em',
    'border-radius': 5,
    background: '#555',
    border: '1px solid #ddd',
};

var caret_style = merge(poss, {
    color: transparent,
    overflow: 'hidden',
    position: absolute,
    margin: 0,
    top: 0,
    left: 0,
});

var textarea_style = merge(poss, { overflow: 'hidden', position: absolute, margin: 0, top: 0, left: 0 });

const toggleConsolePanel = (ctx, shouldExpand) => {
    ctx.editorContainer.els.consolePanel.els.expander.els.button.classList.replace(
        shouldExpand ? 'fa-angle-up' : 'fa-angle-down',
        shouldExpand ? 'fa-angle-down' : 'fa-angle-up',
    );

    ctx.editorContainer.els.editor.style.height = shouldExpand ? 'calc(100vh - 260px)' : 'calc(100vh - 60px)';

    ctx.editorContainer.els.console.style.display = shouldExpand ? 'block' : 'none';
};

const validateContent = (ctx, zendoc) => async (event) => {
    if (event.target.classList.contains('fa-spinner')) {
        return;
    }

    ctx.editorContainer.els.consolePanel.els.expander.els.errors.textContent = 'Problems (0)';

    ctx.editorContainer.els.console.innerHTML = '';

    event.target.classList.replace('fa-square-check', 'fa-spinner');

    event.target.style.animation = 'spin 1s linear infinite';

    try {
        const sanitizedFshDocumentContent = sanitize(ctx.editorContainer.els.editor.els.textarea.value);

        const response = await fetch(zendoc.validator.endpoint, {
            method: 'POST',
            mode: 'cors',

            headers: {
                'Content-Type': 'application/json',
            },

            body: JSON.stringify({
                documentName: zendoc.doc,
                documentContent: sanitizedFshDocumentContent,
            }),
        });

        const { problemsNumber, messageLines, errorMessage } = await response.json();

        if (!response.ok) {
            throw new Error(errorMessage);
        }

        if (problemsNumber > 0) {
            ctx.editorContainer.els.consolePanel.els.expander.els.errors.textContent = `Problems (${problemsNumber})`;

            toggleConsolePanel(ctx, true);
        }

        messageLines.forEach((messageLine) => {
            const preElement = document.createElement('pre');

            preElement.style.marginTop = '0px';
            preElement.style.marginBottom = '0px';

            preElement.textContent = messageLine.length === 0 ? ' ' : messageLine;

            ctx.editorContainer.els.console.append(preElement);
        });
    } catch (error) {
        ctx.editorContainer.els.consolePanel.els.expander.els.errors.textContent = `Problems (1)`;

        toggleConsolePanel(ctx, true);

        const preElement = document.createElement('pre');

        preElement.style.marginTop = '0px';
        preElement.style.marginBottom = '0px';

        preElement.textContent = error.message;

        ctx.editorContainer.els.console.append(preElement);
    } finally {
        event.target.classList.replace('fa-spinner', 'fa-square-check');

        event.target.style.animation = null;
    }
};

var editor = (zendoc) => {
    var symIdx = new quickScore.QuickScore(zendoc.symbols || [], ['name', 'title']);
    symIdx.config.maxIterations = 1000;
    var keysIdx = new quickScore.QuickScore(zendoc.keys || [], ['name']);
    keysIdx.config.maxIterations = 1000;
    var iconsIdx = new quickScore.QuickScore(zendoc.icons || [], ['name', 'title']);
    iconsIdx.config.maxIterations = 1000;
    var annotationsIdx = new quickScore.QuickScore(zendoc.annotations || [], ['name']);
    annotationsIdx.config.maxIterations = 1000;

    var ctx = { symbols: symIdx, keys: keysIdx, icons: iconsIdx, annotations: annotationsIdx, doc: zendoc.doc };
    var in_chrome =
        (window.location.search || '').includes('chrome') || document.body.getBoundingClientRect().width < 800;
    ctx.in_chrome = in_chrome;

    var editor_style = {
        position: relative,
        background: black,
        color: 'white',
        overflow: 'auto',
        height: 'calc(100vh - 60px)',
    };

    var containerConfig = {
        tag: 'div',
        append: document.body,
        style: { display: 'flex', padding: 0 },
    };

    ctx.container = el(containerConfig);

    var on_paste = (evt) => {
        const clipboardItems = evt.clipboardData.items;
        console.log('paste', evt);
        const items = [].slice.call(clipboardItems).filter(function (item) {
            console.log(item);
            return item.type.indexOf('image') !== -1;
        });
        const item = items[0];
        if (item) {
            const blob = item.getAsFile();
            console.log(blob);
            const file_name = prompt('Give file a uniq name', blob.name);
            if (file_name) {
                fetch(`/${ctx.doc}/file`, {
                    method: 'POST',
                    headers: { 'content-type': item.type, 'file-name': file_name },
                    body: blob,
                }).then((resp) => {
                    insert_text(ctx, `((img "${file_name}" {:width 400 :class "screenshot"}))`);
                });
            }
        }
    };

    const hasValidator = !!zendoc.validator;

    if (hasValidator) {
        const styleSheet = document.styleSheets[0];

        styleSheet.insertRule(`
          @keyframes spin {
            0% { transform: rotate(0deg); }
            100% { transform: rotate(360deg); }
          }
        `);
    }

    ctx.editorContainer = el({
        tag: 'div',
        append: ctx.container,

        style: {
            display: 'flex',
            width: '100%',
            height: '100%',
            'flex-direction': 'column',
            flex: 1,
        },

        els: {
            editor: {
                tag: 'div',
                style: editor_style,
                els: {
                    hl: { tag: 'pre', style: merge(poss, {}) },
                    caret: {
                        tag: 'pre',
                        style: caret_style,
                        els: { txt: { tag: 'text' }, cursor: { tag: 'span', text: ' ', style: {} } },
                    },
                    textarea: {
                        tag: 'textarea',
                        spellcheck: false,
                        autofocus: true,
                        on: {
                            keyup: (ev) => {
                                on_editor_keyup(ctx, ev);
                            },
                            paste: on_paste,
                            keydown: (ev) => {
                                on_editor_keydown(ctx, ev);
                            },
                            click: (ev) => {
                                hide_popup(ctx);
                            },
                        },
                        value: zendoc.text || '',
                        style: textarea_style,
                    },
                    pop: { tag: 'div', style: popup_style },
                },
            },
            consolePanel: {
                tag: 'div',

                style: {
                    display: 'flex',
                    'align-items': 'center',
                    'justify-content': 'space-between',
                    height: '60px',
                    width: '100%',
                    'border-top': '1px solid rgba(255, 255, 255, 0.6)',
                    background: 'rgb(31, 41, 55)',
                },

                els: {
                    expander: {
                        tag: 'div',

                        style: {
                            display: 'flex',
                            'align-items': 'center',
                            'justify-content': 'space-between',
                            gap: '16px',
                            padding: '0 16px',
                        },

                        els: !hasValidator
                            ? null
                            : {
                                  button: {
                                      tag: 'i',
                                      class: ['fa-solid', 'fa-angle-up'],

                                      style: {
                                          cursor: 'pointer',
                                          color: 'white',
                                          'font-size': 30,
                                          'z-index': '10000',
                                      },

                                      on: {
                                          click(event) {
                                              const isExpandConsolePanel =
                                                  event.target.classList.contains('fa-angle-down');

                                              toggleConsolePanel(ctx, !isExpandConsolePanel);
                                          },
                                      },
                                  },

                                  title: {
                                      tag: 'span',

                                      style: {
                                          color: 'white',
                                      },

                                      text: 'Console',
                                  },

                                  errors: {
                                      tag: 'span',

                                      style: {
                                          color: 'white',
                                      },

                                      text: 'Problems (0)',
                                  },
                              },
                    },

                    buttons: {
                        tag: 'div',

                        style: {
                            display: 'flex',
                            'align-items': 'center',
                            'justify-content': 'space-between',
                            gap: '16px',
                            padding: '0 16px',
                        },

                        els: {
                            validate: !hasValidator
                                ? null
                                : {
                                      tag: 'i',
                                      class: ['fa-solid', 'fa-square-check'],
                                      style: {
                                          cursor: 'pointer',
                                          'z-index': '10000',
                                          'font-size': 40,
                                          color: 'white',
                                      },
                                      on: {
                                          click: validateContent(ctx, zendoc),
                                      },
                                  },
                            save: {
                                tag: 'i',
                                class: ['fa-solid', 'fa-save'],
                                style: {
                                    cursor: 'pointer',
                                    color: 'white',
                                    'font-size': 40,
                                    'z-index': '10000',
                                },
                                on: {
                                    click: (ev) => {
                                        ev.target.style.visibility = 'hidden';
                                        save(ctx);
                                    },
                                },
                            },
                        },
                    },
                },
            },
            console: {
                tag: 'div',

                style: {
                    display: 'none',
                    height: '200px',
                    width: '40vw',
                    padding: '8px',
                    'border-top': '1px solid rgba(255, 255, 255, 0.6)',
                    background: 'rgb(31, 41, 55)',
                    color: 'white',
                    'font-size': 12,
                    overflow: 'auto',
                },
            },
        },
    });
    hl(ctx, zendoc.text);

    // var preview = new DOMParser().parseFromString(zendoc.preview, "text/html");

    // TODO think if in_chrome is still needed
    if (!in_chrome) {
        ctx.preview = el({
            tag: 'div',
            html: 'loading...', // preview.documentElement.textContent,
            style: { height: 'calc(100vh - 20px)', padding: 20, width: '60vw', overflow: 'auto' },
            append: ctx.container,
        });

        ctx.preview.querySelectorAll('script').forEach((x) => {
            eval(x.innerText);
        });
    }
    window.ctx = ctx;

    render(zendoc.text);
    update_widgets();
    return ctx;
};

main(() => {
    if (window.zendoc) {
        editor(zendoc);
    }
});
