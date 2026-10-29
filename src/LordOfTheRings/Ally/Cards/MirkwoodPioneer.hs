module LordOfTheRings.Ally.Cards.MirkwoodPioneer (mirkwoodPioneer) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Placement (Placement (Unplaced))

newtype MirkwoodPioneer = MirkwoodPioneer AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

mirkwoodPioneer :: AllyCard MirkwoodPioneer
mirkwoodPioneer = ally MirkwoodPioneer Cards.mirkwoodPioneer

-- | You may give it doomed 1 when you play it from your hand. Asked while it is paid for.
instance HasModifiersFor MirkwoodPioneer where
  getModifiersFor (MirkwoodPioneer a) = when (a.placement == Unplaced) $ modified a (CardIdTarget a.allyCardId) [MayGainDoomed 1]

-- | If you do, it gains: "Response: After you play Mirkwood Pioneer, choose a card in the staging area. Until the
-- end of the round, the chosen card does not contribute its threat."
instance HasAbilities MirkwoodPioneer where
  getAbilities (MirkwoodPioneer a) =
    [ restricted a 1 (TargetHasModifier (CardIdTarget a.allyCardId) (GainKeyword (Doomed 1)))
        $ response
        $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)
    ]

instance RunMessage MirkwoodPioneer where
  runMessage msg s@(MirkwoodPioneer a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      enemies <- select EnemyInStagingArea
      locations <- select LocationInStagingArea
      chooseOneM pid do
        questionLabeled "mirkwoodPioneer.choose"
        causedBy a
        targets enemies \e -> roundModifier a e DoesNotContributeThreat
        targets locations \l -> roundModifier a l DoesNotContributeThreat
      pure s
    _ -> MirkwoodPioneer <$> runMessage msg a
