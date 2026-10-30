module LordOfTheRings.Enemy.Cards.WraithOfCarnDum (wraithOfCarnDum) where

import LordOfTheRings.Card (EncounterCard (..), toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Sorcery))

newtype WraithOfCarnDum = WraithOfCarnDum EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wraithOfCarnDum :: EnemyCard WraithOfCarnDum
wraithOfCarnDum = enemy WraithOfCarnDum Cards.wraithOfCarnDum

-- | Defeated, it stays in play for its Forced effect to attach it. As a Sorcery
-- attachment: "Attached character has -1 willpower, -1 attack, and -1 defense."
instance HasModifiersFor WraithOfCarnDum where
  getModifiersFor (WraithOfCarnDum a) = case a.placement of
    AttachedTo host -> do
      modifySelf a [GainTrait Sorcery]
      modified a host [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)]
    _ -> modifySelf a [StaysInPlayWhenDefeated]

{- | Forced: When Wraith of Carn Dûm leaves play, attach it to a character in play. (Counts
as a Sorcery attachment with the text: "... If the attached character leaves play, return
Wraith of Carn Dûm to the staging area as an enemy.")
The first player chooses the character. Read off its defeat: with no character in play it
is discarded.
-}
instance RunMessage WraithOfCarnDum where
  runMessage msg (WraithOfCarnDum attrs) = case msg of
    CheckWindows ws
      | not attached
      , notNull [() | Window After (EnemyDefeated eid) <- ws, eid == attrs.id] -> do
          characters <- selectCharacters AnyHero AnyAlly
          lead <- getLead
          if null characters
            then discard attrs
            else chooseOrRunOneM lead do
              questionLabeled "wraithOfCarnDum.attach"
              causedBy attrs
              targets characters \c -> do
                push $ DiscardShadowCards attrs.id
                placeEnemy attrs.id (AttachedTo c)
          WraithOfCarnDum <$> runMessage msg attrs
    PlaceEnemy eid (AttachedTo _) | eid == attrs.id -> WraithOfCarnDum <$> runMessage msg (attrs & tokensL .~ mempty)
    -- its character left play, and it leaves the discard pile for the staging area
    Discard (EnemyTarget eid) | eid == attrs.id, attached -> do
      addToStagingFromDiscard $ MkEncounterCard attrs.cardId (toCardCode attrs)
      WraithOfCarnDum <$> runMessage msg attrs
    _ -> WraithOfCarnDum <$> runMessage msg attrs
   where
    attached = case attrs.placement of
      AttachedTo _ -> True
      _ -> False
