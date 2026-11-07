module LordOfTheRings.Ally.Cards.HostOfLorien (hostOfLorien) where

import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur.Helpers (giveLorienSupport)
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Objective.CardDefs qualified as Cards

newtype HostOfLorien = HostOfLorien AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

hostOfLorien :: AllyCard HostOfLorien
hostOfLorien = ally HostOfLorien Cards.hostOfLorien

instance HasModifiersFor HostOfLorien where
  getModifiersFor (HostOfLorien a) = modifySelf a [ImmuneToPlayerCardEffects, DoesNotExhaustToCommit, RemoveFromGameIfLeavesPlay]

-- Epic setup reverse: the controlled support can be given at round end.
instance HasAbilities HostOfLorien where
  getAbilities (HostOfLorien a) =
    [restricted a 2 InEpicMultiplayer $ response RoundEndsWindow | InPlayArea _ <- [a.placement]]

instance RunMessage HostOfLorien where
  runMessage msg h@(HostOfLorien a) = case msg of
    UseCardAbility pid (isSource a -> True) 2 _ -> h <$ giveLorienSupport msg pid a
    _ -> HostOfLorien <$> runMessage msg a
