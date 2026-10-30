module LordOfTheRings.Ally.Cards.RangerOfCardolan (rangerOfCardolan) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait (Trait (Dunedain))

newtype RangerOfCardolan = RangerOfCardolan AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Response: After you engage an enemy, if you control at least 1 Dúnedain hero, spend 1
-- resource to put Ranger of Cardolan into play from your hand, under your control. At the
-- end of the round, if Ranger of Cardolan is still in play, shuffle it into its owner's
-- deck.
rangerOfCardolan :: AllyCard RangerOfCardolan
rangerOfCardolan =
  ally
    ( \a ->
        RangerOfCardolan
          a
            { allyHandWindow = Just $ PlayerEngagedEnemyWindow After a.owner
            , allyHandCriteria =
                exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithTrait Dunedain)
                  <> ResourcesOfSphere (PlayerWithId a.owner) Neutral 1
            }
    )
    Cards.rangerOfCardolan

instance RunMessage RangerOfCardolan where
  runMessage msg s@(RangerOfCardolan a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      spendResources pid Neutral 1
      putAllyIntoPlayFromHand pid cid
      roundModifier a (CardIdTarget cid) ShuffleIntoOwnersDeckAtEndOfRound
      pure s
    _ -> RangerOfCardolan <$> runMessage msg a
