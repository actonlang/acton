// Tooltips with a crosshair that snaps to the nearest thread count, and the
// metric switch of each figure. Panel values come from #panel-data.
(function () {
  var tip = document.getElementById('tip');
  var panels = JSON.parse(document.getElementById('panel-data').textContent);
  var NAMES = {acton: 'Acton', go: 'Go', tokio: 'Tokio'};

  function place(x, y) {
    var w = tip.offsetWidth, h = tip.offsetHeight;
    var left = x + 14, top = y + 14;
    if (left + w > window.innerWidth - 8) left = Math.max(8, x - w - 14);
    if (top + h > window.innerHeight - 8) top = Math.max(8, y - h - 14);
    tip.style.left = left + 'px';
    tip.style.top = top + 'px';
  }

  function show(title, rows, x, y) {
    tip.textContent = '';
    var t = document.createElement('div');
    t.className = 'tip-title';
    t.textContent = title;
    tip.appendChild(t);
    rows.forEach(function (r) {
      var row = document.createElement('div');
      row.className = 'tip-row';
      var key = document.createElement('span');
      key.className = 'tip-key ' + r[0];
      var val = document.createElement('strong');
      val.textContent = r[1];
      var lbl = document.createElement('span');
      lbl.className = 'tip-lbl';
      lbl.textContent = r[2];
      row.appendChild(key);
      row.appendChild(val);
      row.appendChild(lbl);
      tip.appendChild(row);
    });
    tip.hidden = false;
    place(x, y);
  }

  function hide() { tip.hidden = true; }

  document.querySelectorAll('svg.sm').forEach(function (svg) {
    var d = panels[svg.getAttribute('data-panel')];
    var figure = svg.closest('[data-metric]');
    var xh = svg.querySelector('.xhair');
    var idx = d.xs.length - 1;
    function at(i, x, y) {
      idx = i;
      var p = d.xs[i];
      xh.setAttribute('x1', p.x);
      xh.setAttribute('x2', p.x);
      xh.setAttribute('visibility', 'visible');
      var second = figure && figure.getAttribute('data-metric') === 'cpu';
      var rows = ['acton', 'go', 'tokio'].filter(function (rt) { return p.v[rt]; }).map(function (rt) {
        var v = p.v[rt];
        return [rt, second ? v[1] : v[0], NAMES[rt] + ', ' + (second ? v[0] : v[1])];
      });
      show(d.title + ', ' + p.t + (p.t === 1 ? ' thread' : ' threads'), rows, x, y);
    }
    function nearest(e) {
      var pt = svg.createSVGPoint();
      pt.x = e.clientX;
      pt.y = e.clientY;
      var loc = pt.matrixTransform(svg.getScreenCTM().inverse());
      var best = 0;
      d.xs.forEach(function (p, i) { if (Math.abs(p.x - loc.x) < Math.abs(d.xs[best].x - loc.x)) best = i; });
      return best;
    }
    function off() { xh.setAttribute('visibility', 'hidden'); hide(); }
    function focusAt(i) {
      var b = svg.getBoundingClientRect();
      at(i, b.left + b.width * d.xs[i].x / 300, b.top);
    }
    var hit = svg.querySelector('.hit');
    hit.addEventListener('pointermove', function (e) { at(nearest(e), e.clientX, e.clientY); });
    hit.addEventListener('pointerleave', off);
    svg.addEventListener('focus', function () { focusAt(idx); });
    svg.addEventListener('blur', off);
    svg.addEventListener('keydown', function (e) {
      if (e.key === 'ArrowLeft' && idx > 0) { focusAt(idx - 1); e.preventDefault(); }
      if (e.key === 'ArrowRight' && idx < d.xs.length - 1) { focusAt(idx + 1); e.preventDefault(); }
    });
  });

  document.querySelectorAll('.seg').forEach(function (seg) {
    var figure = seg.closest('[data-metric]');
    seg.querySelectorAll('button').forEach(function (btn) {
      btn.addEventListener('click', function () {
        figure.setAttribute('data-metric', btn.getAttribute('data-metric'));
        figure.querySelector('.chart-sub').textContent = btn.getAttribute('data-sub');
        seg.querySelectorAll('button').forEach(function (b) {
          b.setAttribute('aria-pressed', String(b === btn));
        });
      });
    });
  });
})();
