function updateAutoRefreshButtonText() {
    document.getElementById('autoRefreshToggle').textContent = isAutoRefreshOn() ? 'auto-refresh UI: enabled' : 'auto-refresh UI: disabled'
}

function isAutoRefreshOn() {
    return window.localStorage.getItem('hoffAutoRefresh') == 'true';
}

function toggleAutoRefresh() {
    window.localStorage.setItem('hoffAutoRefresh', !isAutoRefreshOn());
    updateAutoRefreshButtonText();
}

function main(){
    updateAutoRefreshButtonText();
    window.addEventListener('storage', updateAutoRefreshButtonText); // Synchronize other tabs

    window.setInterval(function () {
        if(isAutoRefreshOn()) { window.location.reload(); }
    }, 5000);
}

main();