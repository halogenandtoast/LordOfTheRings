module LordOfTheRings.Ally.Cards.MirkwoodExplorer (mirkwoodExplorer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype MirkwoodExplorer = MirkwoodExplorer AllyAttrs
  deriving anyclass (IsAlly, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodExplorer :: AllyCard MirkwoodExplorer
mirkwoodExplorer = ally MirkwoodExplorer Cards.mirkwoodExplorer

-- | Response: After Mirkwood Explorer quests successfully, place 1 progress on it.
-- Action: Exhaust Mirkwood Explorer to move all progress from it to a location in play.
instance HasAbilities MirkwoodExplorer where
  getAbilities (MirkwoodExplorer a) =
    [ restricted a 1 (exists (AllyWithId a.id <> CommittedAlly)) $ response (QuestedSuccessfullyWindow After)
    , restricted a 2 (exists AnyLocation) $ ActionAbility (exhaustCost a)
    ]

instance RunMessage MirkwoodExplorer where
  runMessage msg s@(MirkwoodExplorer a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 1
      pure s
    UseCardAbility pid (isSource a -> True) 2 _ -> do
      let progress = countTokens Progress a.allyTokens
      locations <- select LocationNotImmuneToPlayerCards
      chooseOrRunOneM pid do
        questionLabeled "mirkwoodExplorer.location"
        causedBy a
        targets locations \l -> do
          removeTokens a a Progress progress
          placeProgress a l progress
      pure s
    _ -> MirkwoodExplorer <$> runMessage msg a
