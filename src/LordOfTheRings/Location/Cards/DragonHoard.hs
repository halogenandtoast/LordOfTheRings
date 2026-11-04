module LordOfTheRings.Location.Cards.DragonHoard (dragonHoard) where

import LordOfTheRings.Card (Card (PlayerCard), CardType (AttachmentType), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (GuardRelease (AttachWhenFree))
import LordOfTheRings.Trait (Trait (Artifact))

newtype DragonHoard = DragonHoard LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

dragonHoard :: LocationCard DragonHoard
dragonHoard = location DragonHoard Cards.dragonHoard

-- | Immune to player card effects. Fire-drake cannot take damage.
instance HasModifiersFor DragonHoard where
  getModifiersFor (DragonHoard a) = do
    modifySelf a [ImmuneToPlayerCardEffects]
    drakes <- select (EnemyWithCardCode $ toCardCode Enemies.fireDrake)
    for_ drakes \d -> modified a d [CannotTakeDamage]

-- | Response: After Dragon Hoard becomes the active location, the first player chooses a
-- player to attach an Artifact attachment from his hand to Dragon Hoard.
instance HasAbilities DragonHoard where
  getAbilities (DragonHoard a) = [mkAbility a 1 $ response $ LocationBecameActiveWindow After (LocationWithId a.id)]

{- | That attachment gains the guarded (location) keyword: Dragon Hoard guards it, and its
owner attaches it once Dragon Hoard leaves play. If that attachment has the printed
guarded keyword, do not resolve it when the attachment enters play: it is not played, so
its own guard never resolves. The first player chooses among the players with an Artifact
attachment in hand, and that player chooses which.
-}
instance RunMessage DragonHoard where
  runMessage msg l@(DragonHoard a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      players <- getRemainingPlayers
      holding <- for players \p -> (p,) . artifactsIn <$> getPlayer p
      chooseOrRunOneM lead do
        questionLabeled "dragonHoard.choosePlayer"
        causedBy a
        for_ [(p, cs) | (p, cs@(_ : _)) <- holding] \(p, cs) -> targeting p do
          chooseOrRunOneM p do
            questionLabeled "dragonHoard.chooseArtifact"
            causedBy a
            showing (map PlayerCard cs)
            for_ cs \c -> targeting (CardIdTarget c.id) do
              removeFromHand p c
              guardPlayerCard a.cardId AttachWhenFree c
      pure l
    _ -> DragonHoard <$> runMessage msg a
   where
    artifactsIn p = [c | PlayerCard c <- p.hand, cardMatches (CardWithType AttachmentType <> CardWithTrait Artifact) (PlayerCard c)]
