import { useState } from "react";
import { DollarSign, Info } from "lucide-react";

type Roadmap = {
  title: string;
  tokensLent: number;
  completion: number;
  reward: number;
};

export default function Lend() {
  const [lentTokens, setLentTokens] = useState(1500);
  const [heldTokens, setHeldTokens] = useState(5000);
  const [availableTokens, setAvailableTokens] = useState(3500);

  const [showLendModal, setShowLendModal] = useState(false);
  const [lendAmount, setLendAmount] = useState("");
  const [withdrawAmount, setWithdrawAmount] = useState("");
  const [showWithdrawModal, setShowWithdrawModal] = useState(false);
  const [showOverWithdrawModal, setShowOverWithdrawModal] = useState(false);
  const [showInterestWarningModal, setShowInterestWarningModal] =
    useState(false);
  const [showOverLendModal, setShowOverLendModal] = useState(false);

  const [roadmaps, setRoadmaps] = useState<Roadmap[]>([
    {
      title: "Pacific Cleanup Initiative",
      tokensLent: 1000,
      completion: 65,
      reward: 2000,
    },
    {
      title: "Urban Recycling Program",
      tokensLent: 500,
      completion: 30,
      reward: 1000,
    },
  ]);

  const handleLendTokens = () => {
    const amount = parseInt(lendAmount);
    if (isNaN(amount) || amount <= 0) return;

    if (amount > availableTokens) {
      setShowLendModal(false);
      setShowOverLendModal(true); // trigger the warning modal
      return;
    }

    setLentTokens((prev) => prev + amount);
    setAvailableTokens((prev) => prev - amount);
    setLendAmount("");
    setShowLendModal(false);
  };

  const handleWithdraw = () => {
    const amount = parseInt(withdrawAmount);
    setLentTokens((prev) => prev - amount);
    setAvailableTokens((prev) => prev + amount);
    setWithdrawAmount("");
  };

  return (
    <div className="bg-white text-black mx-auto px-4 md:px-10 lg:px-20 py-6 min-h-screen">
      {/* Lending Header */}
      <div className="bg-[#e7eaf7] p-6 rounded-xl mb-8">
        <h1 className="text-2xl font-bold text-[#082FB9] mb-2">
          Put your PLASTIK tokens to use
        </h1>
        <p className="text-[#082FB9]">
          Lend up to 1,000,000 PLASTIK and earn 2% interest when roadmaps are
          completed
        </p>
      </div>

      {/* Stats Grid */}
      <div className="grid grid-cols-2 md:grid-cols-4 gap-4 mb-8">
        <div className="bg-white rounded-2xl shadow-md border border-[#E5E7EB] p-4">
          <h2 className="text-gray-500 text-sm mb-1">PLASTIK Held</h2>
          <p className="text-2xl font-bold">{heldTokens.toLocaleString()}</p>
        </div>
        <div className="bg-white rounded-2xl shadow-md border border-[#E5E7EB] p-4">
          <h2 className="text-gray-500 text-sm mb-1">PLASTIK Lent</h2>
          <p className="text-2xl font-bold">{lentTokens.toLocaleString()}</p>
        </div>
        <div className="bg-white rounded-2xl shadow-md border border-[#E5E7EB] p-4">
          <h2 className="text-gray-500 text-sm mb-1">Available</h2>
          <p className="text-2xl font-bold">
            {(heldTokens - lentTokens).toLocaleString()}
          </p>
        </div>

        <div className="bg-white rounded-2xl shadow-md border border-[#E5E7EB] p-4 flex items-center justify-center">
          <button
            onClick={() => setShowWithdrawModal(true)}
            className="bg-[#082FB9] hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full transition"
          >
            Withdraw
          </button>
        </div>
      </div>

      {/* Roadmap Table */}
      <div className="bg-white rounded-xl shadow-sm overflow-hidden">
        <div className="p-6 border-b">
          <h2 className="text-lg font-semibold">Active Allocations</h2>
        </div>
        <div className="overflow-x-auto">
          <table className="w-full">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-3 text-left text-sm font-medium text-gray-500">
                  Roadmap
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Tokens Lent
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Progress
                </th>
                <th className="px-6 py-3 text-right text-sm font-medium text-gray-500">
                  Reward
                </th>
              </tr>
            </thead>
            <tbody className="divide-y divide-gray-200">
              {roadmaps.map((roadmap, index) => (
                <tr key={index}>
                  <td className="px-6 py-4 text-sm font-medium text-gray-900">
                    {roadmap.title}
                  </td>
                  <td className="px-6 py-4 text-right text-sm text-gray-500">
                    {roadmap.tokensLent.toLocaleString()}
                  </td>
                  <td className="px-6 py-4 text-right">
                    <div className="flex items-center justify-end">
                      <div className="w-32 h-2 bg-gray-200 rounded-full overflow-hidden">
                        <div
                          className="h-full bg-[#082FB9] transition-all"
                          style={{ width: `${roadmap.completion}%` }}
                        />
                      </div>
                      <span className="ml-2 text-sm text-gray-500">
                        {roadmap.completion}%
                      </span>
                    </div>
                  </td>
                  <td className="px-6 py-4 text-right text-sm text-gray-500">
                    <div className="flex items-center justify-end">
                      <DollarSign className="w-4 h-4 mr-1 text-[#082FB9]" />
                      {roadmap.reward.toLocaleString()} USDM
                    </div>
                  </td>
                </tr>
              ))}
            </tbody>
          </table>
        </div>
      </div>

      {/* Lend Modal */}
      {showLendModal && (
        <div className="fixed inset-0 backdrop-blur-md bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-md shadow-md">
            <h3 className="text-lg font-semibold mb-4">Lend PLASTIK Tokens</h3>
            <input
              type="number"
              value={lendAmount}
              onChange={(e) => setLendAmount(e.target.value)}
              placeholder="Enter amount to lend"
              className="w-full p-3 border rounded-lg mb-4"
            />
            <div className="flex gap-4">
              <button
                onClick={() => setShowLendModal(false)}
                className="flex-1 px-4 py-2 border rounded-lg hover:bg-gray-50"
              >
                Cancel
              </button>
              <button
                onClick={handleLendTokens}
                className="flex-1 px-4 py-2 bg-green-600 text-white rounded-lg hover:bg-green-700"
              >
                Confirm
              </button>
            </div>
          </div>
        </div>
      )}
      {/* Over Lend Modal */}
      {showOverLendModal && (
        <div className="fixed inset-0 backdrop-blur-md bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-md shadow-md">
            <div className="flex items-center mb-4">
              <Info className="w-6 h-6 text-red-500 mr-2" />
              <h3 className="text-lg font-semibold text-red-600">
                Lending Error
              </h3>
            </div>
            <p className="text-gray-700 mb-6">
              The amount you entered exceeds your available tokens.
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => setShowOverLendModal(false)}
                className="flex-1 px-4 py-2 border rounded-lg hover:bg-gray-50"
              >
                Close
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Withdraw Modal */}
      {showWithdrawModal && (
        <div className="fixed inset-0 backdrop-blur-md bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-md shadow-md">
            <h3 className="text-lg font-semibold mb-4">
              Withdraw PLASTIK Tokens
            </h3>
            <input
              type="number"
              value={withdrawAmount}
              onChange={(e) => setWithdrawAmount(e.target.value)}
              placeholder="Enter amount to withdraw"
              className="w-full p-3 border rounded-lg mb-4"
            />
            <div className="flex gap-4">
              <button
                onClick={() => {
                  setWithdrawAmount("");
                  setShowWithdrawModal(false);
                }}
                className="flex-1 px-4 py-2 border rounded-lg hover:bg-gray-50"
              >
                Cancel
              </button>
              <button
                onClick={() => {
                  const amount = parseInt(withdrawAmount);
                  if (isNaN(amount) || amount <= 0) return;
                  if (amount > lentTokens) {
                    setShowWithdrawModal(false);
                    setShowOverWithdrawModal(true);
                  } else {
                    setShowWithdrawModal(false);
                    setShowInterestWarningModal(true);
                  }
                }}
                className="flex-1 px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700"
              >
                Continue
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Interest Warning Modal */}
      {showInterestWarningModal && (
        <div className="fixed inset-0 backdrop-blur-md  bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-md shadow-md">
            <div className="flex items-center mb-4">
              <Info className="w-6 h-6 text-yellow-500 mr-2" />
              <h3 className="text-lg font-semibold">Withdrawal Warning</h3>
            </div>
            <p className="text-gray-600 mb-6">
              Some of your tokens are currently funding a roadmap. Retrieving
              them now means you'll lose the potential 2% interest.
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => setShowInterestWarningModal(false)}
                className="flex-1 px-4 py-2 border rounded-lg hover:bg-gray-50"
              >
                Cancel
              </button>
              <button
                onClick={() => {
                  handleWithdraw();
                  setShowInterestWarningModal(false);
                }}
                className="flex-1 px-4 py-2 bg-red-600 text-white rounded-lg hover:bg-red-700"
              >
                Continue
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Over Withdraw Modal */}
      {showOverWithdrawModal && (
        <div className="fixed inset-0 backdrop-blur-md bg-opacity-50 flex items-center justify-center p-4">
          <div className="bg-white rounded-xl p-6 w-full max-w-md shadow-md">
            <div className="flex items-center mb-4">
              <Info className="w-6 h-6 text-red-500 mr-2" />
              <h3 className="text-lg font-semibold text-red-600">
                Withdrawal Error
              </h3>
            </div>
            <p className="text-gray-700 mb-6">
              The Entered amount exceeds your available token balance.
            </p>
            <div className="flex gap-4">
              <button
                onClick={() => setShowOverWithdrawModal(false)}
                className="flex-1 px-4 py-2 border rounded-lg hover:bg-gray-50"
              >
                Close
              </button>
            </div>
          </div>
        </div>
      )}

      {/* Floating Lending Button */}
      <div className="flex justify-center mt-5">
        <button
          className="bg-[#082FB9] hover:bg-blue-700 text-white font-semibold px-5 py-2.5 rounded-full transition mt-2"
          onClick={() => setShowLendModal(true)}
        >
          Lend Tokens
        </button>
      </div>
    </div>
  );
}
