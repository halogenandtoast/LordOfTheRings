module LordOfTheRings.Ally.Cards.ElfhelmTactics (elfhelmTactics) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype ElfhelmTactics = ElfhelmTactics AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rohanHeroWithResources :: PlayerId -> HeroMatcher
rohanHeroWithResources pid = HeroControlledBy (PlayerWithId pid) <> HeroWithTrait Rohan <> HeroWithResources

-- | Action: Spend 1 resource from a Rohan hero's resource pool to put Elfhelm into play
-- from your hand under your control. At the end of the phase, discard Elfhelm if he is
-- still in play.
elfhelmTactics :: AllyCard ElfhelmTactics
elfhelmTactics =
  ally
    (\a -> ElfhelmTactics a {allyHandAction = Just $ exists (rohanHeroWithResources a.owner)})
    Cards.elfhelmTactics

instance RunMessage ElfhelmTactics where
  runMessage msg s@(ElfhelmTactics a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      heroes <- select (rohanHeroWithResources pid)
      chooseOrRunOneM pid do
        questionLabeled "elfhelm.chooseHero"
        causedBy a
        targets heroes \hid -> do
          spendResourcesFrom pid hid 1
          putAllyIntoPlayFromHand pid cid
          phaseModifier a (CardIdTarget cid) DiscardAtEndOfPhase
      pure s
    _ -> ElfhelmTactics <$> runMessage msg a
