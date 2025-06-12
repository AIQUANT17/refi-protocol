// import { useContext, useEffect, useState } from "react";
// import Button from "../../components/Button";
// import CommunityStats from "../../components/community/CommunityStats";
// import TokenBalance from "../../components/community/TokenBalance";
// import StartVote from "../../components/community/StartVote";
// import VotingPopup from "../../components/community/VotingPopup";
// import { toast } from "sonner";
// import { WalletContext } from "../../App";

// const stats = {
//   currentRetirementRate: "2%",
//   totalVotes: "15,672",
//   totalPlastikVoted: "45.2M",
//   remainingTime: "2d 6h 32m",
// };

// const percentages = [2, 3, 4, 5];
// const Community: React.FC = () => {
//   const [plastikBalance, setPlastikBalance] = useState<number>(0);
//   const [votingAllowed, setVotingAllowed] = useState<boolean>(false);
//   const [eligibleforVoting, setEligibleforVoting] = useState<boolean>(false);
//   const [voting, setVoting] = useState<boolean>(false);
//   const [votingPopup, setVotingPopup] = useState<boolean>(false);
//   const [votingPercentage, setVotingPercentage] = useState<number>(0);

//   // Data from Environment Variables
//   const plastikTokenAddress = import.meta.env.VITE_PLASTIK_TOKEN_ADDRESS;
//   const minimumPlastikToInitializeVoting = import.meta.env.VITE_MINIMUM_PLASTIK;
//   const minimumPlastikToVote = import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE;

//   const wallet = useContext(WalletContext);
//   const fetchBalances = async () => {
//     if (!wallet) return;
//     const balances = await wallet.getBalance();
//     console.log(balances);
//     console.log("plastikTokenAddress", plastikTokenAddress);

//     const plastik = balances.find(
//       (item) => item.unit === plastikTokenAddress
//     )?.quantity;
//     console.log(plastik);
//     setPlastikBalance(plastik ? Number(plastik) : 0);
//     // Check if user is eligible to vote
//     setVotingAllowed(
//       plastik ? Number(plastik) >= minimumPlastikToInitializeVoting : false
//     );
//     setEligibleforVoting(
//       plastik ? Number(plastik) >= minimumPlastikToVote : false
//     );
//   };

//   const startVoting = () => {
//     toast.success("Success message!");
//     setVotingPopup(false);
//     setVoting(true);
//     console.log("Voting started");
//   };

//   useEffect(() => {
//     if (wallet === null) {
//       setPlastikBalance(0);
//       setVotingAllowed(false);
//       setEligibleforVoting(false);
//       setVoting(false);
//       setVotingPercentage(0);
//       return;
//     }
//     fetchBalances();
//   }, [wallet]);
//   const votingDuration = 30;

//   const votingStartDate = new Date();
//   const votingEndDate = new Date();
//   votingEndDate.setDate(votingStartDate.getDate() + votingDuration);

//   const formatDate = (date: Date) =>
//     date.toLocaleDateString("en-US", {
//       month: "long",
//       day: "numeric",
//       year: "numeric",
//     });

//   const votingPeriodFormatted = `${formatDate(votingStartDate)} - ${formatDate(
//     votingEndDate
//   )}`;

//   return (
//     <div className="bg-gray-50 text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
//       <h1 className="text-2xl font-bold mb-2">Community Voting</h1>
//       <p className="mb-6 text-sm text-gray-600">
//         Create or participate in votes to determine token retirement percentage.
//         Participate in shaping the future of Plastiks ecosystem.
//       </p>
//       {/* Current Statistics */}
//       {/* Current Statistics in 4 Cards */}
//       <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
//         <div className="bg-white p-4 rounded-lg shadow border text-center">
//           <p className="text-sm text-gray-500">Last Winning Rate</p>
//           <p className="text-xl font-bold">{stats.currentRetirementRate}</p>
//           <p>Token Retirement</p>
//         </div>
//         <div className="bg-white p-4 rounded-lg shadow border text-center">
//           <p className="text-sm text-gray-500">Round Duration</p>
//           <p className="text-gray-600">
//             The voting round will last for {votingDuration} days:{" "}
//             {votingPeriodFormatted}
//           </p>
//           <p>2025</p>
//         </div>
//         <div className="bg-white p-4 rounded-lg shadow border text-center">
//           <p className="text-sm text-gray-500">Total Voters</p>
//           <p className="text-xl font-bold">{stats.totalVotes}</p>
//         </div>
//         <div className="bg-white p-4 rounded-lg shadow border text-center">
//           <p className="text-sm text-gray-500">Total Token Retired</p>
//           <p className="text-xl font-bold">{stats.remainingTime}</p>
//         </div>
//       </div>

//       {votingAllowed && (
//         <div className="bg-[#F1F4FE] p-4 rounded-lg mb-6">
//           <h2 className="text-black font-medium mb-2">Minimum requirements</h2>
//           <div className="flex flex-col gap-1">
//             <p className="text-gray-600 text-sm">
//               You can participate by voting for you preferred token retirement
//               rate below. A new round will begin once this one ends
//             </p>
//             <p className="text-gray-600 text-sm">
//               Holds atleast {minimumPlastikToInitializeVoting} plastik Tokens
//             </p>
//             <p className="text-gray-600 text-sm">
//               {votingDuration}-day voting period
//             </p>
//           </div>
//         </div>
//       )}

//       {votingAllowed ? (
//         <div className="flex gap-6">
//           <div className="bg-white p-4 rounded-lg border border-gray-200 mb-6 w-full">
//             <h1 className="text-center text-2xl font-semibold mb-4">
//               Start New Voting Round
//             </h1>
//             {/* Token Balance */}
//             <TokenBalance
//               plastikBalance={plastikBalance}
//               votingAllowed={votingAllowed}
//             />
//             <p>Select Retirement Percentage</p>
//             <div className="flex items-center gap-4 mt-4">
//               {percentages.map((percentage) => (
//                 <Button
//                   key={percentage}
//                   variant="userButton"
//                   className={`w-1/4 text-2xl border border-gray-300 rounded-lg font-semibold cursor-pointer ${
//                     votingPercentage == percentage
//                       ? "bg-black text-gray-100"
//                       : "text-black"
//                   }`}
//                   onClick={() => setVotingPercentage(percentage)}
//                 >
//                   {percentage}%
//                 </Button>
//               ))}
//             </div>
//             <div className="bg-gray-200 p-4 rounded-lg mt-4">
//               <h2 className="text-black text-xl font-medium">
//                 Voting Duration
//               </h2>
//               <p className="text-gray-600">{votingStartDate}</p>
//             </div>
//             <Button
//               variant="userButton"
//               className="w-[19%] m-auto mt-4 bg-[#082FB9] text-white font-semibold rounded-3xl"
//               onClick={() => setVotingPopup(true)}
//             >
//               Start Voting Round
//             </Button>
//           </div>
//           {/* <div className="w-1/3 bg-white p-4 rounded-lg border border-gray-200 mb-6">
//             <CommunityStats stats={stats} votingAllowed={votingAllowed} />
//           </div> */}
//         </div>
//       ) : (
//         <div>
//           <TokenBalance
//             plastikBalance={plastikBalance}
//             votingAllowed={votingAllowed}
//           />
//         </div>
//       )}

//       {/* Token Retirement Rate Vote */}
//       {voting && (
//         <StartVote
//           eligibleforVoting={eligibleforVoting}
//           oldRetirementRate={2}
//           newRetirementRate={votingPercentage}
//           oldRetirementPercentage={20}
//           newRetirementPercentage={50}
//         />
//       )}

//       {votingPopup && (
//         <VotingPopup
//           votingPercentage={votingPercentage}
//           votingDuration={votingDuration}
//           onStartVoting={startVoting}
//           onClose={() => setVotingPopup(false)}
//         />
//       )}
//     </div>
//   );
// };

// export default Community;

// import { useContext, useEffect, useState } from "react";
// import Button from "../../components/Button";
// import TokenBalance from "../../components/community/TokenBalance";
// import StartVote from "../../components/community/StartVote";
// import VotingPopup from "../../components/community/VotingPopup";
// import { toast } from "sonner";
// import { WalletContext } from "../../App";

// const stats = {
//   currentRetirementRate: "2%",
//   totalVotes: "15,672",
//   totalPlastikVoted: "45.2M",
//   remainingTime: "2d 6h 32m",
// };

// const percentages = [2, 3, 4, 5];

// const Community: React.FC = () => {
//   const [plastikBalance, setPlastikBalance] = useState<number>(0);
//   const [votingAllowed, setVotingAllowed] = useState<boolean>(false);
//   const [eligibleForVoting, setEligibleForVoting] = useState<boolean>(false);
//   const [voting, setVoting] = useState<boolean>(false);
//   const [votingPopup, setVotingPopup] = useState<boolean>(false);
//   const [votingPercentage, setVotingPercentage] = useState<number>(0);

//   const plastikTokenAddress = import.meta.env.VITE_PLASTIK_TOKEN_ADDRESS;
//   const minimumPlastikToInitializeVoting = Number(
//     import.meta.env.VITE_MINIMUM_PLASTIK
//   );
//   const minimumPlastikToVote = Number(
//     import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE
//   );

//   const wallet = useContext(WalletContext);

//   const fetchBalances = async () => {
//     if (!wallet) return;
//     const balances = await wallet.getBalance();
//     const plastik = balances.find(
//       (item) => item.unit === plastikTokenAddress
//     )?.quantity;
//     const plastikAmount = plastik ? Number(plastik) : 0;
//     setPlastikBalance(plastikAmount);
//     setVotingAllowed(plastikAmount >= minimumPlastikToInitializeVoting);
//     setEligibleForVoting(plastikAmount >= minimumPlastikToVote);
//   };

//   const startVoting = () => {
//     toast.success("Voting round started successfully!");
//     setVotingPopup(false);
//     setVoting(true);
//   };

//   useEffect(() => {
//     if (!wallet) {
//       setPlastikBalance(0);
//       setVotingAllowed(false);
//       setEligibleForVoting(false);
//       setVoting(false);
//       setVotingPercentage(0);
//       return;
//     }
//     fetchBalances();
//   }, [wallet]);

//   const votingDuration = 30;
//   const votingStartDate = new Date();
//   const votingEndDate = new Date(votingStartDate);
//   votingEndDate.setDate(votingEndDate.getDate() + votingDuration);

//   const formatDate = (date: Date) =>
//     date.toLocaleDateString("en-US", {
//       month: "long",
//       day: "numeric",
//     });
//   const getYear = (date: Date) => date.getFullYear();
//   const votingPeriodFormatted = `${formatDate(votingStartDate)} - ${formatDate(
//     votingEndDate
//   )}`;
//   const votingYear = getYear(votingStartDate);
//   return (
//     <div className="bg-gray-50 text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
//       <h1 className="text-2xl font-bold mb-2">Community Voting</h1>
//       <p className="mb-6 text-sm text-gray-600">
//         Create or participate in votes to determine token retirement percentage.
//         Participate in shaping the future of Plastiks ecosystem.
//       </p>

//       <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
//         <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
//           <p className="text-xl">Last Winning Rate</p>
//           <p className="text-xl font-semibold">{stats.currentRetirementRate}</p>
//           <p>Token Retirement</p>
//         </div>
//         <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
//           <p className="text-xl ">Round Duration</p>
//           <p className="text-[#0D0D0D] font-semibold text-xl">
//             {votingPeriodFormatted}
//           </p>
//           <p className="text-[#737373]">{votingYear}</p>
//         </div>
//         <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
//           <p className="text-xl">Total Voters</p>
//           <p className="text-xl font-semibold text-[#0D0D0D]">
//             {stats.totalVotes}
//           </p>
//         </div>
//         <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
//           <p className="text-xl">Total Token Retired</p>
//           <p className="text-xl font-semibold text-[#0D0D0D]">
//             {stats.remainingTime}
//           </p>
//           <p className="font-medium text-[#737373]">PLASTIK</p>
//         </div>
//       </div>

//       {votingAllowed && (
//         <div className="bg-[#F1F4FE] p-4 rounded-lg mb-6 ">
//           <h2 className="text-[#0D0D0D] font-medium mb-2">
//             Minimum requirements
//           </h2>
//           <div className="flex flex-col gap-1">
//             <p className="text-gray-600 text-sm">
//               You can participate by voting for your preferred token retirement
//               rate below. A new round will begin once this one ends.
//             </p>
//             <p className="text-gray-600 text-sm">
//               Hold at least {minimumPlastikToInitializeVoting} Plastik tokens
//             </p>
//             <p className="text-gray-600 text-sm">
//               {votingDuration}-day voting period
//             </p>
//           </div>
//         </div>
//       )}

//       {votingAllowed ? (
//         <div className="flex gap-6">
//           <div className="bg-white p-4 rounded-lg border border-gray-200 mb-6 w-full">
//             <div>
//               <TokenBalance
//                 plastikBalance={plastikBalance}
//                 votingAllowed={votingAllowed}
//               />
//             </div>
//             <h1 className="text-center text-2xl font-semibold mb-4">
//               Start New Voting Round
//             </h1>
//             <TokenBalance
//               plastikBalance={plastikBalance}
//               votingAllowed={votingAllowed}
//             />
//             <p>Select Retirement Percentage</p>
//             <div className="flex items-center gap-4 mt-4">
//               {percentages.map((percentage) => (
//                 <Button
//                   key={percentage}
//                   variant="userButton"
//                   className={`w-1/4 text-2xl border border-gray-300 rounded-lg font-semibold cursor-pointer ${
//                     votingPercentage === percentage
//                       ? "bg-black text-gray-100"
//                       : "text-black"
//                   }`}
//                   onClick={() => setVotingPercentage(percentage)}
//                 >
//                   {percentage}%
//                 </Button>
//               ))}
//             </div>
//             <div className="bg-gray-200 p-4 rounded-lg mt-4">
//               <h2 className="text-black text-xl font-medium">
//                 Voting Duration
//               </h2>
//               <p className="text-gray-600">
//                 This Voting round will last for 30 days until{" "}
//                 {formatDate(votingEndDate)} {getYear(votingEndDate)}
//               </p>
//             </div>
//             <Button
//               variant="userButton"
//               className="w-[19%] m-auto mt-4 bg-[#082FB9] text-white font-semibold rounded-3xl"
//               onClick={() => setVotingPopup(true)}
//             >
//               Start Voting Round
//             </Button>
//           </div>
//         </div>
//       ) : (
//         <div>
//           <TokenBalance
//             plastikBalance={plastikBalance}
//             votingAllowed={votingAllowed}
//           />
//         </div>
//       )}

//       {voting && (
//         <StartVote
//           eligibleforVoting={eligibleForVoting}
//           oldRetirementRate={2}
//           newRetirementRate={votingPercentage}
//           oldRetirementPercentage={20}
//           newRetirementPercentage={50}
//         />
//       )}

//       {votingPopup && (
//         <VotingPopup
//           votingPercentage={votingPercentage}
//           votingDuration={votingDuration}
//           onStartVoting={startVoting}
//           onClose={() => setVotingPopup(false)}
//         />
//       )}
//     </div>
//   );
// };

// export default Community;

import { useContext, useEffect, useState } from "react";
import Button from "../../components/Button";
import TokenBalance from "../../components/community/TokenBalance";
import StartVote from "../../components/community/StartVote";
import VotingPopup from "../../components/community/VotingPopup";
import { toast } from "sonner";
import { WalletContext } from "../../App";

const stats = {
  currentRetirementRate: "2%",
  totalVotes: "15,672",
  totalPlastikVoted: "45.2M",
  remainingTime: "2d 6h 32m",
};

const percentages = [2, 3, 4, 5];

const Community: React.FC = () => {
  const [plastikBalance, setPlastikBalance] = useState<number>(0);
  const [votingAllowed, setVotingAllowed] = useState<boolean>(false);
  const [eligibleForVoting, setEligibleForVoting] = useState<boolean>(false);
  const [voting, setVoting] = useState<boolean>(false);
  const [votingPopup, setVotingPopup] = useState<boolean>(false);
  const [votingPercentage, setVotingPercentage] = useState<number>(0);

  const plastikTokenAddress = import.meta.env.VITE_PLASTIK_TOKEN_ADDRESS;
  const minimumPlastikToInitializeVoting = Number(
    import.meta.env.VITE_MINIMUM_PLASTIK
  );
  const minimumPlastikToVote = Number(
    import.meta.env.VITE_MINIMUM_PLASTIK_TO_VOTE
  );

  const wallet = useContext(WalletContext);

  const fetchBalances = async () => {
    if (!wallet) return;
    const balances = await wallet.getBalance();
    const plastik = balances.find(
      (item) => item.unit === plastikTokenAddress
    )?.quantity;
    const plastikAmount = plastik ? Number(plastik) : 0;
    setPlastikBalance(plastikAmount);
    setVotingAllowed(plastikAmount >= minimumPlastikToInitializeVoting);
    setEligibleForVoting(plastikAmount >= minimumPlastikToVote);
  };

  const startVoting = () => {
    toast.success("Voting round started successfully!");
    setVotingPopup(false);
    setVoting(true);
  };

  useEffect(() => {
    if (!wallet) {
      setPlastikBalance(0);
      setVotingAllowed(false);
      setEligibleForVoting(false);
      setVoting(false);
      setVotingPercentage(0);
      return;
    }
    fetchBalances();
  }, [wallet]);

  const votingDuration = 30;
  const votingStartDate = new Date();
  const votingEndDate = new Date(votingStartDate);
  votingEndDate.setDate(votingEndDate.getDate() + votingDuration);

  const formatDate = (date: Date) =>
    date.toLocaleDateString("en-US", {
      month: "long",
      day: "numeric",
    });
  const getYear = (date: Date) => date.getFullYear();
  const votingPeriodFormatted = `${formatDate(votingStartDate)} - ${formatDate(
    votingEndDate
  )}`;
  const votingYear = getYear(votingStartDate);

  return (
    <div className="bg-gray-50 text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-[calc(80vh)]">
      <h1 className="text-2xl font-bold mb-2">Community Voting</h1>
      <p className="mb-6 text-sm text-gray-600">
        Create or participate in votes to determine token retirement percentage.
        Participate in shaping the future of Plastiks ecosystem.
      </p>

      <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4 mb-6">
        <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
          <p className="text-xl">Last Winning Rate</p>
          <p className="text-xl font-semibold">{stats.currentRetirementRate}</p>
          <p>Token Retirement</p>
        </div>
        <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
          <p className="text-xl ">Round Duration</p>
          <p className="text-[#0D0D0D] font-semibold text-xl">
            {votingPeriodFormatted}
          </p>
          <p className="text-[#737373]">{votingYear}</p>
        </div>
        <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
          <p className="text-xl">Total Voters</p>
          <p className="text-xl font-semibold text-[#0D0D0D]">
            {stats.totalVotes}
          </p>
        </div>
        <div className="bg-[#FFFFFF] p-4 rounded-2xl shadow  text-start">
          <p className="text-xl">Total Token Retired</p>
          <p className="text-xl font-semibold text-[#0D0D0D]">
            {stats.remainingTime}
          </p>
          <p className="font-medium text-[#737373]">PLASTIK</p>
        </div>
      </div>

      {votingAllowed && (
        <div className="bg-[#F1F4FE] p-4 rounded-lg mb-6 ">
          <h2 className="text-[#0D0D0D] font-medium mb-2">
            Minimum requirements
          </h2>
          <div className="flex flex-col gap-1">
            <p className="text-gray-600 text-sm">
              You can participate by voting for your preferred token retirement
              rate below. A new round will begin once this one ends.
            </p>
            <p className="text-gray-600 text-sm">
              Hold at least {minimumPlastikToInitializeVoting} Plastik tokens
            </p>
            <p className="text-gray-600 text-sm">
              {votingDuration}-day voting period
            </p>
          </div>
        </div>
      )}
      {/* Token Balance shown on top */}
      <div className="mb-4">
        <TokenBalance
          plastikBalance={plastikBalance}
          votingAllowed={votingAllowed}
        />
      </div>
      {votingAllowed ? (
        <div className="flex gap-6">
          <div className="bg-white p-4 rounded-lg border border-gray-200 mb-6 w-full">
            <h1 className="text-center text-2xl font-semibold mb-4">
              Start New Voting Round
            </h1>
            <p className="font-semibold">Select Retirement Percentage</p>
            <div className="flex items-center gap-4 mt-4">
              {percentages.map((percentage) => (
                <Button
                  key={percentage}
                  variant="userButton"
                  className={`w-1/4 text-2xl border border-gray-300 rounded-lg font-semibold cursor-pointer ${
                    votingPercentage === percentage
                      ? "bg-black text-gray-100"
                      : "text-black"
                  }`}
                  onClick={() => setVotingPercentage(percentage)}
                >
                  {percentage}%
                </Button>
              ))}
            </div>
            <div className="bg-gray-200 p-4 rounded-lg mt-4">
              <h2 className="text-black text-xl font-medium">
                Voting Duration
              </h2>
              <p className="text-gray-600">
                This Voting round will last for 30 days until{" "}
                {formatDate(votingEndDate)}, {getYear(votingEndDate)}
              </p>
            </div>
            <Button
              variant="userButton"
              className="w-[19%] m-auto mt-4 bg-[#082FB9] text-white font-semibold rounded-3xl"
              onClick={() => setVotingPopup(true)}
            >
              Start Voting Round
            </Button>
          </div>
        </div>
      ) : (
        ""
      )}

      {voting && (
        <StartVote
          eligibleforVoting={eligibleForVoting}
          oldRetirementRate={2}
          newRetirementRate={votingPercentage}
          oldRetirementPercentage={20}
          newRetirementPercentage={50}
        />
      )}

      {votingPopup && (
        <VotingPopup
          votingPercentage={votingPercentage}
          votingDuration={votingDuration}
          onStartVoting={startVoting}
          onClose={() => setVotingPopup(false)}
        />
      )}
    </div>
  );
};

export default Community;
