// Confaive - Connect Five with Artificial Intelligence
// Sound-free browser port of the 1999 QBasic original.
// Board and AI scoring logic ported from confaive.bas
// (SUBs/FUNCTIONs: ClearBoard, DrawBoard, CalculatePoints, Density, CalculateBest, WinCheck).

const COLS = 30;
const ROWS = 18;
const CELL = 10;
const ORIGIN = 9; // top-left offset of the board, matches original DrawBoard

const EMPTY = 0, HUMAN = 1, COMPUTER = 2;

const canvas = document.getElementById('board');
const ctx = canvas.getContext('2d');

const statusEl = document.getElementById('status');
const modeSel = document.getElementById('mode');
const difficultySel = document.getElementById('difficulty');
const strategySel = document.getElementById('strategy');
const newGameBtn = document.getElementById('newGame');
const undoBtn = document.getElementById('undo');

let board; // 1..30 x 1..18 grid, EMPTY/HUMAN/COMPUTER
let turn;
let moves;
let history;
let gameOver;
let aiTimer = null;

// Reads a board cell; cells outside the playable 30x18 area act as a
// permanent wall (value 9), exactly like the "9" border in the original.
function B(x, y) {
  if (x < 1 || x > COLS || y < 1 || y > ROWS) return 9;
  return board[x][y];
}

function newGame() {
  board = [];
  for (let x = 0; x <= COLS + 1; x++) {
    board[x] = new Array(ROWS + 2).fill(EMPTY);
  }
  turn = HUMAN;
  moves = 0;
  history = [];
  gameOver = false;
  if (aiTimer) { clearTimeout(aiTimer); aiTimer = null; }
  draw();
  updateStatus();
  maybeTriggerComputer();
}

function place(x, y, player) {
  board[x][y] = player;
  history.push({ x, y });
  moves++;
}

function undoOne() {
  const last = history.pop();
  if (last) {
    board[last.x][last.y] = EMPTY;
    moves--;
  }
}

function undo() {
  if (gameOver) { gameOver = false; }
  if (aiTimer) { clearTimeout(aiTimer); aiTimer = null; }
  const mode = modeSel.value;
  // Undo the computer's move and the player's move together in PvC mode.
  const stepsBack = mode === 'pvc' && history.length >= 2 ? 2 : 1;
  for (let i = 0; i < stepsBack && history.length; i++) undoOne();
  turn = HUMAN;
  draw();
  updateStatus();
}

// --- Win detection (functionally equivalent to the original WinCheck) ---
function checkWin(px, py, player) {
  const dirs = [[1, 0], [0, 1], [1, 1], [1, -1]];
  for (const [dx, dy] of dirs) {
    let cells = [[px, py]];
    for (let s = 1; s < 5; s++) {
      const x = px + dx * s, y = py + dy * s;
      if (B(x, y) === player) cells.push([x, y]); else break;
    }
    for (let s = 1; s < 5; s++) {
      const x = px - dx * s, y = py - dy * s;
      if (B(x, y) === player) cells.unshift([x, y]); else break;
    }
    if (cells.length >= 5) return cells;
  }
  return null;
}

// --- AI: ported from CalculatePoints() in confaive.bas ---
function countSymbol(str, symbol) {
  let c = 0;
  for (let i = 0; i < str.length; i++) if (str[i] === symbol) c++;
  return c;
}

function calculatePoints(x, y, st1, st2, turnValue, densCalc) {
  const dir1 = [[], [], [], []];
  for (let j = 0; j < 4; j++) {
    const d = new Array(9);
    for (let i = 1; i <= 9; i++) {
      let v;
      switch (j) {
        case 0: v = B(x - 5 + i, y + 5 - i); break;
        case 1: v = B(x - 5 + i, y); break;
        case 2: v = B(x - 5 + i, y - 5 + i); break;
        case 3: v = B(x, y - 5 + i); break;
      }
      d[i - 1] = String(v);
    }
    dir1[j] = d;
  }
  const dir2 = dir1.map(arr => arr.slice());

  for (let j = 0; j < 4; j++) {
    dir1[j][4] = '1'; // simulate the human playing (x,y)
    dir2[j][4] = '2'; // simulate the computer playing (x,y)
  }

  let points = 0, possCorner1 = 0, possCorner2 = 0;

  for (let j = 0; j < 4; j++) {
    let l1 = 0, r1 = 0, l2 = 0, r2 = 0;
    for (let i = 1; i <= 4; i++) {
      if (dir1[j][4 + i] === '2') r1 = 1;
      if (dir1[j][4 - i] === '2') l1 = 1;
      if (r1 === 1) dir1[j][4 + i] = '9';
      if (l1 === 1) dir1[j][4 - i] = '9';
      if (dir2[j][4 + i] === '1') r2 = 1;
      if (dir2[j][4 - i] === '1') l2 = 1;
      if (r2 === 1) dir2[j][4 + i] = '9';
      if (l2 === 1) dir2[j][4 - i] = '9';
    }
    const s1 = dir1[j].join('');
    const s2 = dir2[j].join('');

    if (s1.includes('011110') || s2.includes('022220')) points += 2 * Math.pow(4, 8);
    if (s1.includes('01110')) {
      possCorner1++;
      if (possCorner1 >= 2) points += 2 * Math.pow(4, 8);
    }
    if (s1.includes('02220')) {
      possCorner2++;
      if (possCorner2 >= 2) points += 2 * Math.pow(4, 8);
    }
    if (s1.includes('911119') || s2.includes('922229')) points -= 2 * Math.pow(4, 8);
    if (s1.includes('91119') || s2.includes('92229')) points -= 2 * Math.pow(3, 8);
  }

  for (let j = 0; j < 4; j++) {
    const s1 = dir1[j].join('');
    const s2 = dir2[j].join('');
    for (let i = 0; i < 5; i++) {
      let p = countSymbol(s1.substr(i, 5), '1');
      points += Math.pow(p, 8) + st1 * Math.pow(p, 4);
      p = countSymbol(s2.substr(i, 5), '2');
      if (turnValue === COMPUTER && p === 5) p = 6;
      points += Math.pow(p, 8) + st2 * Math.pow(p, 4);
    }
  }

  if (densCalc) points += density(x, y, turnValue);
  return points;
}

// --- AI: ported from Density() in confaive.bas ---
function density(x, y, turnValue) {
  let pPoints = 0, cPoints = 0;
  for (let i = 4; i >= 1; i--) {
    for (let j = x - i; j <= x + i; j++) {
      const above = B(j, y + i), below = B(j, y - i);
      if (above === HUMAN) pPoints += Math.pow(5 - i, 2);
      if (above === COMPUTER) cPoints += Math.pow(5 - i, 2);
      if (below === HUMAN) pPoints += Math.pow(5 - i, 2);
      if (below === COMPUTER) cPoints += Math.pow(5 - i, 2);
    }
    for (let j = y - i + 1; j <= y + i - 1; j++) {
      const left = B(x - i, j), right = B(x + i, j);
      if (left === HUMAN) pPoints += Math.pow(5 - i, 2);
      if (left === COMPUTER) cPoints += Math.pow(5 - i, 2);
      if (right === HUMAN) pPoints += Math.pow(5 - i, 2);
      if (right === COMPUTER) cPoints += Math.pow(5 - i, 2);
    }
  }

  const strategy = Number(strategySel.value);
  let points;
  if (turnValue === HUMAN) {
    points = strategy === 1 ? pPoints + 2 * cPoints
      : strategy === 2 ? 2 * pPoints + cPoints
      : 1.5 * pPoints + 1.5 * cPoints;
  } else {
    points = strategy === 1 ? 2 * pPoints + cPoints
      : strategy === 2 ? pPoints + 2 * cPoints
      : 1.5 * pPoints + 1.5 * cPoints;
  }
  return Math.round(points * 2);
}

// --- AI: ported from CalculateBest() in confaive.bas ---
function calculateBest() {
  if (moves === 0) {
    return {
      x: Math.floor(Math.random() * 8) + 12,
      y: Math.floor(Math.random() * 6) + 6
    };
  }

  const randomIQ = Number(difficultySel.value);
  const strategy = Number(strategySel.value);
  const scores = [];
  let best = -Infinity;

  const fullyRandom = Math.floor(Math.random() * 101) > randomIQ;

  let st1 = 0, st2 = 0;
  if (!fullyRandom) {
    if (turn === HUMAN) { st1 = strategy === 1 ? -1 : strategy === 2 ? 1 : 0; st2 = strategy === 1 ? 1 : strategy === 2 ? -1 : 0; }
    else { st1 = strategy === 1 ? 1 : strategy === 2 ? -1 : 0; st2 = strategy === 1 ? -1 : strategy === 2 ? 1 : 0; }
  }

  const densCalc = true;
  for (let x = 1; x <= COLS; x++) {
    for (let y = 1; y <= ROWS; y++) {
      if (board[x][y] !== EMPTY) continue;
      let score;
      if (fullyRandom) {
        score = Math.floor(Math.random() * 1000);
      } else {
        score = calculatePoints(x, y, st1, st2, turn, densCalc) + Math.floor(Math.random() * 1000);
      }
      if (score > best) { best = score; scores.length = 0; scores.push({ x, y }); }
      else if (score === best) { scores.push({ x, y }); }
    }
  }

  if (scores.length === 0) return null; // board full
  return scores[Math.floor(Math.random() * scores.length)];
}

// --- Rendering ---
function draw() {
  ctx.fillStyle = '#000';
  ctx.fillRect(0, 0, canvas.width, canvas.height);

  ctx.strokeStyle = '#444';
  ctx.lineWidth = 1;
  for (let i = 0; i <= COLS; i++) {
    const px = ORIGIN + i * CELL + 0.5;
    ctx.beginPath();
    ctx.moveTo(px, ORIGIN);
    ctx.lineTo(px, ORIGIN + ROWS * CELL);
    ctx.stroke();
  }
  for (let j = 0; j <= ROWS; j++) {
    const py = ORIGIN + j * CELL + 0.5;
    ctx.beginPath();
    ctx.moveTo(ORIGIN, py);
    ctx.lineTo(ORIGIN + COLS * CELL, py);
    ctx.stroke();
  }

  for (let x = 1; x <= COLS; x++) {
    for (let y = 1; y <= ROWS; y++) {
      const v = board[x][y];
      if (v === EMPTY) continue;
      const cx = ORIGIN + (x - 1) * CELL + CELL / 2;
      const cy = ORIGIN + (y - 1) * CELL + CELL / 2;
      ctx.fillStyle = v === HUMAN ? '#ff5555' : '#55aaff';
      ctx.font = 'bold 9px monospace';
      ctx.textAlign = 'center';
      ctx.textBaseline = 'middle';
      ctx.fillText(v === HUMAN ? 'X' : 'O', cx, cy + 1);
    }
  }
}

function highlightWin(cells) {
  ctx.strokeStyle = '#fff';
  ctx.lineWidth = 2;
  for (const [x, y] of cells) {
    ctx.strokeRect(
      ORIGIN + (x - 1) * CELL + 1,
      ORIGIN + (y - 1) * CELL + 1,
      CELL - 2, CELL - 2
    );
  }
}

function updateStatus(message) {
  if (message) { statusEl.textContent = message; return; }
  if (gameOver) return;
  if (modeSel.value === 'cvc') statusEl.textContent = 'Computer vs Computer...';
  else statusEl.textContent = turn === HUMAN ? 'Your turn' : 'Computer thinking...';
}

function endGame(winner, cells) {
  gameOver = true;
  draw();
  if (cells) highlightWin(cells);
  const label = winner === HUMAN ? 'You win!' : 'Computer wins!';
  updateStatus(modeSel.value === 'pvp' ? `Player ${winner} wins!` : label);
}

function applyMove(x, y, player) {
  place(x, y, player);
  draw();
  const win = checkWin(x, y, player);
  if (win) { endGame(player, win); return true; }
  if (moves >= COLS * ROWS) { updateStatus('Board full - draw!'); gameOver = true; return true; }
  return false;
}

function computerMove() {
  const move = calculateBest();
  if (!move) return;
  const finished = applyMove(move.x, move.y, turn);
  if (!finished) {
    turn = turn === HUMAN ? COMPUTER : HUMAN;
    updateStatus();
    maybeTriggerComputer();
  }
}

function maybeTriggerComputer() {
  if (gameOver) return;
  const mode = modeSel.value;
  const shouldAiMove = (mode === 'pvc' && turn === COMPUTER) || mode === 'cvc';
  if (shouldAiMove) {
    aiTimer = setTimeout(computerMove, 250);
  }
}

canvas.addEventListener('click', (e) => {
  if (gameOver) return;
  const mode = modeSel.value;
  if (mode === 'cvc') return;
  if (mode === 'pvc' && turn !== HUMAN) return;

  const rect = canvas.getBoundingClientRect();
  const scaleX = canvas.width / rect.width;
  const scaleY = canvas.height / rect.height;
  const px = (e.clientX - rect.left) * scaleX;
  const py = (e.clientY - rect.top) * scaleY;

  const x = Math.floor((px - ORIGIN) / CELL) + 1;
  const y = Math.floor((py - ORIGIN) / CELL) + 1;
  if (x < 1 || x > COLS || y < 1 || y > ROWS) return;
  if (board[x][y] !== EMPTY) return;

  const player = turn;
  const finished = applyMove(x, y, player);
  if (!finished) {
    turn = mode === 'pvp' ? (player === HUMAN ? COMPUTER : HUMAN) : COMPUTER;
    updateStatus();
    maybeTriggerComputer();
  }
});

newGameBtn.addEventListener('click', newGame);
undoBtn.addEventListener('click', undo);
modeSel.addEventListener('change', newGame);

newGame();
