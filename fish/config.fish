if status is-interactive
    # Commands to run in interactive sessions can go here
end

set -U fish_greeting

alias odsmock="cd ~/personal_workspace/order-delivery-schedule/app && npm run serve:mock"
alias odsdock="cd ~/personal_workspace/order-delivery-schedule && docker compose down && docker compose up --build"
alias odsbuild="cd ~/personal_workspace/order-delivery-schedule && mvn clean package -DskipTests"
