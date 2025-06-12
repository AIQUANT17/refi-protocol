import {
  faCircleCheck,
  faXmarkCircle,
} from "@fortawesome/free-solid-svg-icons";
import Button from "../Button";

interface TokenBalanceProps {
  plastikBalance: number;
  votingAllowed: boolean;
}

const TokenBalance = ({ plastikBalance, votingAllowed }: TokenBalanceProps) => {
  return (
    <div className="bg-[#F4F4F4] p-4 rounded-2xl mb-6 flex justify-between border border-[#E5E7EB]">
      <div>
        <h2 className="text-lg font-semibold">Your Token Balance</h2>
        <p className="text-2xl font-bold">
          {new Intl.NumberFormat("en-US").format(plastikBalance)} Plastik
        </p>
      </div>
      <Button
        variant="userButton"
        className={
          votingAllowed
            ? "rounded-full bg-[#A7FE8A] text-[#1B1B1F] gap-1 h-16"
            : "rounded-full bg-[#FFDC85] text-black gap-1 h-16"
        }
        icon={votingAllowed ? faCircleCheck : faXmarkCircle}
      >
        {votingAllowed
          ? "Eligible to Start Voting Round"
          : "Not Eligible to vote"}
      </Button>
    </div>
  );
};

export default TokenBalance;
